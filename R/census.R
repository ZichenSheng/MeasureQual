.failure_classes <- c("ZERO_CAPACITY","INSUFFICIENT_LEGAL_UNITS","CONSTRAINT_FAILURE","GENERATOR_ERROR","TRUTH_ACCESS_VIOLATION","INVALID_INPUT","RESOURCE_LIMIT","UNKNOWN_FAILURE")
.metric <- function(spec,world) {
 v<-.call(attr(spec,"callbacks")$metric_fn,list(world=world,x=world,data=world))
 if(!is.numeric(v)||!length(v)||any(!is.finite(v))) .abort("metric_fn must return a finite numeric vector")
 ag<-attr(spec$unit_spec,"callbacks")$aggregation_fn
 if(!is.null(ag)) v<-.call(ag,list(x=v,values=v,world=world,spec=spec))
 if(!is.numeric(v)||!length(v)||any(!is.finite(v))) .abort("Invalid aggregated metric output")
 v
}
.attempt_world <- function(spec,data,q,seed) {
 tryCatch({
  out<-.generator(spec,data,q,seed)
  if(!is.list(out)||!out$status%in%c("CONSTRUCTED","FAILED")) .abort("Generator must return structured status and world")
  if(out$status=="FAILED") {
   if(is.null(out$failure_class)||!out$failure_class%in%.failure_classes) .abort("Structured refusal needs a known failure_class")
   return(list(constructed=FALSE,valid=FALSE,failure_class=out$failure_class,detail=out$failure_detail))
  }
  cs<-attr(spec$alternative_spec,"constraint_callbacks")
  for(n in names(cs)) {
   ok<-.call(cs[[n]],list(world=out$world,spec=spec,data=out$world))
   if(!identical(ok,TRUE)) return(list(constructed=TRUE,valid=FALSE,failure_class="CONSTRAINT_FAILURE",detail=n))
  }
  metric<-.metric(spec,out$world)
  list(constructed=TRUE,valid=TRUE,failure_class=NA_character_,detail=NA_character_,metric=metric)
 },error=function(e) list(constructed=FALSE,valid=FALSE,failure_class="GENERATOR_ERROR",detail=conditionMessage(e)))
}
.cp_lower <- function(valid,scheduled,alpha) if(valid==0L) 0 else stats::qbeta(alpha,valid,scheduled-valid+1)
.cell_capacity <- function(spec,data,q,attempts,alpha) {
 a<-spec$alternative_spec;oracle<-attr(a,"callbacks")$capacity_fn
 basis<-"EMPIRICAL_EXHAUSTION";detail<-list();zero<-FALSE
 if(!is.null(oracle)) {
  out<-.call(oracle,list(accessible=.accessible(spec,data),severity=q,constraints=attr(a,"constraint_callbacks"),spec=spec,data=data))
  if(!is.list(out)||!is.logical(out$has_capacity)||length(out$has_capacity)!=1L||is.na(out$has_capacity)||!is.list(out$detail)) .abort("capacity_fn must return has_capacity, capacity_basis, detail")
  .choice(out$capacity_basis,c("ANALYTIC_PRE_ATTEMPT","EMPIRICAL_EXHAUSTION"),"capacity_basis")
  zero<-!out$has_capacity;basis<-out$capacity_basis;detail<-out$detail
 }
 ledger<-list()
 if(!zero) for(w in seq_len(attempts)) {
  seed<-.derive_seed(spec$spec_hash,spec$context$context_id,a$name,q,w)
  out<-.attempt_world(spec,data,q,seed)
  ledger[[length(ledger)+1L]]<-list(world_index=w,attempt=1L,constructed=out$constructed,valid=out$valid,failure_class=out$failure_class,detail=out$detail)
  if(out$failure_class%in%c("TRUTH_ACCESS_VIOLATION","INVALID_INPUT")) .abort("Run-level failure during construction","evaluably_error_truth_leakage")
 }
 constructed<-sum(vapply(ledger,function(x) isTRUE(x$constructed),logical(1)))
 valid<-sum(vapply(ledger,function(x) isTRUE(x$valid),logical(1)))
 failures<-vapply(ledger,function(x) if(is.na(x$failure_class)) "" else x$failure_class,character(1))
 lower<-.cp_lower(valid,attempts,alpha)
 gate<-if(valid==0L) "ZERO_CAPACITY" else if(is.null(spec$min_constructibility)) "UNRESOLVED" else if(lower>=spec$min_constructibility) "SUPPORTED" else "INSUFFICIENT"
 hist<-as.list(table(factor(failures[failures!=""],levels=.failure_classes)))
 if(zero) hist$ZERO_CAPACITY<-attempts
 list(severity=q,scheduled=attempts,constructed=constructed,valid=valid,failed=attempts-valid,
 yield_construction=constructed/attempts,yield_validity=if(constructed) valid/constructed else NA_real_,yield_overall=valid/attempts,
 lower_bound=lower,capacity_basis=basis,capacity_detail=detail,attempts_made=length(ledger),failure_class_histogram=hist,capacity_gate=gate,ledger=ledger)
}
.inferential_support <- function(spec,data) {
 r<-spec$inferential_rule;b<-r$reachability_basis
 out<-list(support_cardinality=NA_integer_,min_attainable_p=NA_real_,alpha=r$alpha,alpha_reachable=NA,
  reachability_basis=b,pvalue_convention=r$pvalue_convention,support_gate="UNDETERMINED")
 if(b=="UNDETERMINED") .abort("Inferential reachability is undetermined","evaluably_error_reachability_undetermined")
 if(b=="NOT_APPLICABLE") {out$alpha_reachable<-TRUE;out$support_gate<-"REACHABLE";return(out)}
 v<-if(b=="DECLARED_EXTERNAL") r$external_support else attr(r,"runtime_support_fn")(spec,data)
 if(!is.list(v)||is.null(v$min_attainable_p)) .abort("Support callback requires min_attainable_p")
 .scalar_number(v$min_attainable_p,0,1,"min_attainable_p")
 if(b=="EXACT_ENUMERATION"&&(is.null(v$support_cardinality)||is.na(v$support_cardinality))) .abort("Enumeration requires support_cardinality")
 if(!is.null(v$support_cardinality)&&!is.na(v$support_cardinality)) .int(v$support_cardinality,"support_cardinality",0)
 out$support_cardinality<-if(is.null(v$support_cardinality)) NA_integer_ else v$support_cardinality
 out$min_attainable_p<-v$min_attainable_p;out$alpha_reachable<-out$min_attainable_p<=r$alpha
 out$support_gate<-if(out$alpha_reachable) "REACHABLE" else "ALPHA_UNREACHABLE";out
}
#' Census Legal Alternative Construction
#'
#' Constructs and validates worlds per severity cell, preserving failures and denominators. Capacity and inferential support are separate. The package never derives attainable p-values from arrangement counts.
#' @param spec audit_spec object.
#' @param data Runtime input with named accessible/protected objects and realized id_map.
#' @param attempts Scheduled construction attempts per cell; technical default min(100,n_worlds).
#' @param alpha One-sided construction-bound alpha; technical default 0.05, recorded separately from inferential alpha.
#' @param integrity Integrity receipt; cannot replace a conflicting executed receipt.
#' @return constructibility_census with separate capacity and inferential_support blocks.
#' @section Errors:
#' Malformed contracts raise structured evaluably conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(evaluably)
#' source(system.file("examples", "toy-workflow.R", package = "evaluably"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' constructibility_census(s, t$data)
#' @export
constructibility_census <- function(spec,data,attempts=min(100L,spec$n_worlds),alpha=0.05,integrity=NULL) {
 .ensure_class(spec,"audit_spec");.require_verified(spec);attempts<-.int(attempts,"attempts");.scalar_number(alpha,0,1,"census alpha",TRUE)
 unit<-.unit_gate(spec,data);empty<-.not_assessed(spec)
 x<-c(empty,list(spec_hash=spec$spec_hash,severity_grid=spec$severity_grid,alpha=alpha,unit_gate=unit$gate,
  endpoint_gate="NOT_ASSESSED",entry_gate="BLOCKED",flags=character(),cells=list()))
 if(unit$gate!="LEGAL") return(structure(x,class="constructibility_census"))
 x$endpoint_gate<-spec$endpoint_compatibility_declared
 if(x$endpoint_gate!="SUPPORTED") return(structure(x,class="constructibility_census"))
 if(is.null(integrity)) integrity<-check_alternative_integrity(spec,data)
 x$integrity<-integrity
 if(integrity$outcome=="INVALID_TRUTH_LEAKAGE") return(structure(x,class="constructibility_census"))
 for(q in spec$severity_grid) {
  cell<-.cell_capacity(spec,data,q,attempts,alpha)
  x$cells[[length(x$cells)+1L]]<-cell
  if(cell$capacity_gate=="ZERO_CAPACITY") break
 }
 gates<-vapply(x$cells,`[[`,character(1),"capacity_gate")
 # Family gate is a logical conjunction; cell counts are never pooled.
 family<-if("ZERO_CAPACITY"%in%gates) "ZERO_CAPACITY" else if("INSUFFICIENT"%in%gates) "INSUFFICIENT" else if("UNRESOLVED"%in%gates) "UNRESOLVED" else "SUPPORTED"
 x$capacity<-list(capacity_gate=family,capacity_basis=unique(vapply(x$cells,`[[`,character(1),"capacity_basis")),
  n_blocks=unit$n_blocks,n_units_per_block=unit$n_units_per_block,n_blocks_with_capacity=unit$n_blocks_with_capacity,cells=x$cells)
 if(family=="ZERO_CAPACITY") return(structure(x,class="constructibility_census"))
 if(family=="INSUFFICIENT") x$flags<-"LOW_CONSTRUCTIBILITY"
 x$inferential_support<-.inferential_support(spec,data)
 if(x$inferential_support$support_gate=="ALPHA_UNREACHABLE") x$flags<-c(x$flags,"ALPHA_UNREACHABLE")
 if(spec$inferential_rule$reachability_basis=="DECLARED_EXTERNAL") x$flags<-c(x$flags,"HUMAN_REVIEW_REQUIRED")
 x$entry_gate<-if(x$inferential_support$support_gate=="REACHABLE") "PASS" else "BLOCKED"
 structure(x,class="constructibility_census")
}
