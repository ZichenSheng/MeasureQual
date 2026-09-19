.unit_data <- function(spec,data) {
 u<-spec$unit_spec;f<-attr(u,"callbacks")$id_map
 m<-if(!is.null(f)) f(data) else if(is.data.frame(data)) data else data$id_map
 if(is.null(m)) .abort("data must expose the realized id_map, or unit_spec must declare an id_map callback")
 .validate_id_map(m,u$nesting,unique(c(u$nesting$child,u$nesting$parent)),function(m) .abort(m,"evaluably_error_illegal_unit"))
 if(!is.null(u$id_map)) for(n in unique(c(u$nesting$child,u$nesting$parent)))
  if(!all(m[[n]]%in%u$id_map[[n]])) .abort(paste("Dataset has undeclared unit IDs:",n),"evaluably_error_illegal_unit")
 m
}
.unit_gate <- function(spec,data) {
 m<-.unit_data(spec,data);u<-spec$unit_spec
 n<-length(unique(m[[u$scientific]]));blocks<-split(m[[u$scientific]],m[[u$blocking]])
 counts<-vapply(blocks,function(x) length(unique(x)),integer(1))
 list(gate=if(n>0L&&sum(counts>0L)>=2L) "LEGAL" else "NO_LEGAL_UNIT",n_scientific=n,
 n_blocks=length(counts),n_units_per_block=if(length(counts)) c(min=min(counts),median=stats::median(counts),max=max(counts)) else c(min=0,median=0,max=0),
 n_blocks_with_capacity=sum(counts>0L),scientific_ids=as.character(unique(m[[u$scientific]])))
}
.derive_seed <- function(spec_hash,context,alternative,severity,world_index,seed_context=NULL) {
 h<-.hash(list(spec_hash=spec_hash,context=context,alternative=alternative,severity=severity,world_index=world_index,seed_context=seed_context))
 as.integer(strtoi(substr(h,1L,7L),16L))
}
.with_seed <- function(seed,fn) {
 kind<-RNGkind();had<-exists(".Random.seed",globalenv(),inherits=FALSE)
 old<-if(had) get(".Random.seed",globalenv()) else NULL
 on.exit({do.call(RNGkind,as.list(kind));if(had) assign(".Random.seed",old,globalenv()) else if(exists(".Random.seed",globalenv(),inherits=FALSE)) rm(".Random.seed",envir=globalenv())},add=TRUE)
 RNGkind("L'Ecuyer-CMRG");set.seed(seed);fn()
}
.call <- function(fn,args) {
 if(!is.function(fn)) .abort("Missing callback")
 n<-names(formals(fn));if(!"..."%in%n) args<-args[intersect(names(args),n)]
 do.call(fn,args)
}
.accessible <- function(spec,data) {
 n<-spec$alternative_spec$generator_accessible
 if(!all(n%in%names(data))) .abort("Dataset is missing declared generator-accessible objects")
 data[n]
}
.generator <- function(spec,data,q,seed,fn=NULL) {
 a<-spec$alternative_spec
 if(q==0&&!is.null(attr(spec,"callbacks")$null_generator)&&is.null(fn)) {
  out<-.with_seed(seed,function() .call(attr(spec,"callbacks")$null_generator,list(accessible=.accessible(spec,data),seed=seed)))
  return(list(status="CONSTRUCTED",world=out))
 }
 if(is.null(fn)) fn<-attr(a,"callbacks")$generator_fn
 .with_seed(seed,function() .call(fn,list(accessible=.accessible(spec,data),severity=q,seed=seed,constraints=attr(a,"constraint_callbacks"))))
}
.protected_bindings <- function(fn,protected,seen=list()) {
 if(any(vapply(seen,function(g) rlang::is_reference(fn,g),logical(1)))) return(character())
 g<-codetools::findGlobals(fn,merge=TRUE);out<-character()
 for(s in g) {
  b<-.binding(s,environment(fn));if(is.null(b)) next
  if(.binding_package(b$env)%in%.auto_packages) next
  if(is.function(b$value)) out<-c(out,.protected_bindings(b$value,protected,c(seen,list(fn))))
  else for(n in names(protected)) if(identical(b$value,protected[[n]])) out<-c(out,paste0(s," [",n,"]"))
 }
 unique(out)
}
.probe_clone <- function(fn,protected,probe,seen=list()) {
 # Isolated lexical clone: never assign into the author's environment.
 if(length(seen)>5L) return(fn)
 env<-new.env(parent=baseenv());g<-codetools::findGlobals(fn,merge=TRUE)
 for(s in g) {
  b<-.binding(s,environment(fn));if(is.null(b)) next
  value<-b$value
  if(is.function(value)&&!.binding_package(b$env)%in%.auto_packages) value<-.probe_clone(value,protected,probe,c(seen,list(fn)))
  else for(n in names(protected)) if(identical(value,protected[[n]])) {
   if(is.numeric(value)) value<-value+probe+seq_along(value)
   else if(is.logical(value)) value<-!value
   else if(is.character(value)) value<-paste0(value,"_probe_",probe)
   else if(is.list(value)) value<-rev(value)
  }
  assign(s,value,env)
 }
 f<-fn;environment(f)<-env;f
}
#' Probe the Declared Truth Boundary
#'
#' Checks protected closure bindings and differential generator behavior under a fixed seed. PASS_PROBED is a falsification receipt, not proof of independence. External or reflective access may be unverifiable.
#' @param spec audit_spec object.
#' @param data Runtime input with named accessible/protected objects and realized id_map.
#' @param n_probes Number of integrity probes; technical default 32.
#' @param strict FALSE for the MVP in-process probe; strict subprocess mode is not enabled.
#' @return Plain integrity receipt with outcome, probe count, flagged symbols and flags.
#' @section Errors:
#' Malformed contracts raise structured MeasureQual conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(MeasureQual)
#' source(system.file("examples", "toy-workflow.R", package = "MeasureQual"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' check_alternative_integrity(s, t$data)
#' @export
check_alternative_integrity <- function(spec,data,n_probes=32L,strict=FALSE) {
 .ensure_class(spec,"audit_spec");n_probes<-.int(n_probes,"n_probes",2)
 a<-spec$alternative_spec;fn<-attr(a,"callbacks")$generator_fn
 pn<-unique(c(a$protected_evaluation_truth,a$protected_assignment))
 if(!all(pn%in%names(data))) .abort("Dataset is missing declared protected objects")
 protected<-data[pn];symbols<-.protected_bindings(fn,protected)
 if(a$truth_dependence_declared) {
  .warn("Declared truth dependence requires human review","evaluably_warning_declared_truth_dependence")
  return(list(outcome="UNVERIFIABLE",n_probes=0L,symbols_flagged=symbols,flags=c("DECLARED_TRUTH_DEPENDENCE","HUMAN_REVIEW_REQUIRED"),declaration="DECLARED_TRUTH_DEPENDENCE"))
 }
 if(length(symbols)) return(list(outcome="INVALID_TRUTH_LEAKAGE",n_probes=0L,symbols_flagged=symbols,flags=character()))
 if(!length(pn)) return(list(outcome="UNVERIFIABLE",n_probes=0L,symbols_flagged=character(),flags="FIREWALL_INACTIVE",reason="No protected objects declared"))
 if(strict) .abort("Strict subprocess isolation is not enabled in the MVP; use the declared in-process probe")
 q<-max(spec$severity_grid);seed<-.derive_seed(spec$spec_hash,spec$context$context_id,a$name,q,0L)
 probe_call<-function(f) tryCatch(.generator(spec,data,q,seed,f),error=function(e) list(.probe_error=TRUE,message=conditionMessage(e)))
 baseline<-probe_call(fn);again<-probe_call(fn)
 if(!identical(baseline,again)||isTRUE(baseline$.probe_error)) {
  .warn("Generator determinism could not be verified","evaluably_warning_nondeterministic_generator")
  return(list(outcome="UNVERIFIABLE",n_probes=2L,symbols_flagged=character(),flags="FIREWALL_INACTIVE"))
 }
 dangerous<-c("get","mget","eval","parse","globalenv",".GlobalEnv","sys.frame","parent.frame","readRDS","readLines","source","system","system2")
 if(length(intersect(codetools::findGlobals(fn,merge=TRUE),dangerous))) return(list(outcome="UNVERIFIABLE",n_probes=2L,symbols_flagged=character(),flags="FIREWALL_INACTIVE",reason="Reflective/external access is not probe-verifiable"))
 for(i in seq_len(n_probes)) {
  got<-probe_call(.probe_clone(fn,protected,i))
  if(isTRUE(got$.probe_error)) return(list(outcome="UNVERIFIABLE",n_probes=i,symbols_flagged=character(),flags="FIREWALL_INACTIVE"))
  if(!identical(got,baseline)) return(list(outcome="INVALID_TRUTH_LEAKAGE",n_probes=i,symbols_flagged="differential_probe",flags=character()))
 }
 list(outcome="PASS_PROBED",n_probes=n_probes,symbols_flagged=character(),flags=character())
}
.not_assessed <- function(spec) list(capacity=list(capacity_gate="NOT_ASSESSED"),inferential_support=list(support_gate="NOT_ASSESSED",alpha=spec$inferential_rule$alpha))
.check_shell <- function(spec,data) {
 list(spec=spec,runtime_data_fingerprint=.runtime_fingerprint(data),unit_gate="NOT_ASSESSED",endpoint_gate="NOT_ASSESSED",
 integrity=list(outcome="NOT_ASSESSED",n_probes=NA_integer_,symbols_flagged=character(),flags=character()),census=NULL,
 capacity_gate="NOT_ASSESSED",support_gate="NOT_ASSESSED",entry_gate="PASS",blocking_gate=NA_character_,stop_reason=NA_character_,preterminal_state=NULL,checked_at=.now(),flags=character())
}
.block <- function(x,gate,reason,state="NOT_EVALUABLE") {
 x$entry_gate<-"BLOCKED";x$blocking_gate<-gate;x$stop_reason<-reason;x$preterminal_state<-state
 structure(x,class="audit_check")
}
#' Run Fixed-Order Preflight Gates
#'
#' Verifies the frozen specification, then unit, endpoint, integrity, capacity and inferential support. After the first blocking gate every later gate is NOT_ASSESSED. No raw data are stored in the receipt.
#' @param spec audit_spec object.
#' @param data Runtime input with named accessible/protected objects and realized id_map.
#' @param ... Additional arguments forwarded to the documented next phase.
#' @param on_undetermined Only error is supported.
#' @return Transient audit_check; blocked checks can emit durable evidence records.
#' @section Errors:
#' Malformed contracts raise structured MeasureQual conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(MeasureQual)
#' source(system.file("examples", "toy-workflow.R", package = "MeasureQual"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' check_audit(s, t$data)
#' @export
check_audit <- function(spec,data,...,on_undetermined="error") {
 .ensure_class(spec,"audit_spec");.choice(on_undetermined,"error","on_undetermined")
 if(spec$lifecycle!="FROZEN_CONFIRMATORY") .abort("check_audit requires a frozen confirmatory spec","evaluably_error_not_frozen")
 .require_verified(spec);x<-.check_shell(spec,data)
 x$unit_details<-.unit_gate(spec,data);x$unit_gate<-x$unit_details$gate
 if(x$unit_gate=="NO_LEGAL_UNIT") return(.block(x,"unit_gate","NO_LEGAL_UNIT"))
 x$endpoint_gate<-spec$endpoint_compatibility_declared
 if(x$endpoint_gate!="SUPPORTED") {
  if(x$endpoint_gate=="UNDETERMINED") x$flags<-"HUMAN_REVIEW_REQUIRED"
  return(.block(x,"endpoint_gate",if(x$endpoint_gate=="INCOMPATIBLE") "ENDPOINT_INCOMPATIBLE" else "HUMAN_REVIEW"))
 }
 x$integrity<-check_alternative_integrity(spec,data)
 x$flags<-unique(c(x$flags,x$integrity$flags))
 if(x$integrity$outcome=="INVALID_TRUTH_LEAKAGE") return(.block(x,"integrity","INVALID_TRUTH_LEAKAGE","INVALID"))
 x$census<-constructibility_census(spec,data,...,integrity=x$integrity)
 x$capacity_gate<-x$census$capacity$capacity_gate
 if(x$capacity_gate=="ZERO_CAPACITY") {x$flags<-unique(c(x$flags,"ZERO_CAPACITY_PRESENT"));return(.block(x,"capacity_gate","ZERO_CAPACITY"))}
 x$support_gate<-x$census$inferential_support$support_gate
 x$flags<-unique(c(x$flags,x$census$flags))
 if(x$support_gate=="ALPHA_UNREACHABLE") return(.block(x,"support_gate","ALPHA_UNREACHABLE"))
 structure(x,class="audit_check")
}
