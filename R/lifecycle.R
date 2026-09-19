.exclusion_list <- function() c("description","contact","citation","notes","title")
.spec_payload <- function(x) {
 y<-.plain(x)
 for(n in c("lifecycle","amendment_ledger","parent_spec_hash","spec_hash","frozen_at","audit_member_key",.exclusion_list())) y[n]<-NULL
 if(!is.null(y$subject)) y$subject$subject_label<-NULL
 y$reference_spec$source<-NULL
 y
}
.verify_spec <- function(x) {
 .forbidden_keys(x);.validate_callbacks(x)
 for(n in c("representation_spec","unit_spec","reference_spec","alternative_spec","inferential_rule","decision_map")) {
  o<-x[[n]];.validate_callbacks(o)
  field<-if(n=="representation_spec") "representation_hash" else paste0(n,"_hash")
  exclusions<-c(field,if(n=="representation_spec") "representation_rule_hash" else character())
  if(!identical(.hash(.payload(o)[setdiff(names(.payload(o)),exclusions)]),o[[field]])) return(FALSE)
 }
 if(!identical(x$representation_rule_hash,.hash(.plain(x$representation_spec)[c("object_type","aggregation_rule","transformation","preprocessing","missing_feature_policy")]))) return(FALSE)
 for(n in c("representation_spec","unit_spec","reference_spec","alternative_spec","inferential_rule","decision_map")) {
  field <- if(n=="representation_spec") "representation_hash" else paste0(n,"_hash")
  if(!identical(x[[field]],x[[n]][[field]])) return(FALSE)
 }
 if(!identical(x$metric_hash,x$metric_fn_hash)) return(FALSE)
 if(!identical(x$audit_member_key,.audit_member_key(.member_components(x)))) return(FALSE)
 if(!is.null(x$denominator_contract)) .validate_denominator_contract(x$denominator_contract,x)
 if(!is.null(x$spec_hash)&&!identical(x$spec_hash,.hash(.spec_payload(x)))) return(FALSE)
 TRUE
}
#' Verify Audit and Evidence Hashes
#'
#' Recomputes hashes and current callback dependencies. Verification checks identity, not biological validity. Record paths are read as JSON. Forbidden inheritance keys always error.
#' @param x Audit object or evidence JSON path where supported.
#' @return Invisible scalar logical; FALSE reports a verification mismatch.
#' @section Errors:
#' Malformed contracts raise structured MeasureQual conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' verify_audit(list())
#' @export
verify_audit <- function(x) {
 if(is.character(x)&&length(x)==1L&&file.exists(x)) x<-jsonlite::fromJSON(x,simplifyVector=FALSE)
 .forbidden_keys(x)
 answer<-tryCatch({
  if(inherits(x,"audit_spec")) .verify_spec(x)
  else if(inherits(x,"evidence_record")||is.list(x)&&all(c("identity","provenance","qualification")%in%names(x))) .verify_record(x)
  else if(inherits(x,c("audit_run","audit_result","audit_check"))) {
   ok<-.verify_spec(x$spec)
   if(inherits(x,"audit_run")) ok<-ok&&identical(x$run_hash,.hash(.run_payload(x)))
   if(inherits(x,"audit_result")) ok<-ok&&identical(x$result_hash,.hash(.plain(x)[setdiff(names(x),c("spec","result_hash"))]))
   ok
  } else FALSE
 },error=function(e) FALSE)
 invisible(isTRUE(answer))
}
.require_verified <- function(x) if(!isTRUE(verify_audit(x))) .abort("Frozen audit verification failed","evaluably_error_freeze_verification")
#' Freeze a Pre-Run Audit Declaration
#'
#' Verifies declarations and named thresholds before confirmation. A lineage that has executed cannot be restored to confirmatory status.
#' @param spec audit_spec object.
#' @param decision_map Explicit decision map declaration; no scientific value is inferred by the engine.
#' @return Frozen audit_spec.
#' @section Errors:
#' Malformed contracts raise structured MeasureQual conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(MeasureQual)
#' source(system.file("examples", "toy-workflow.R", package = "MeasureQual"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' freeze_audit(t$spec)
#' @export
freeze_audit <- function(spec, decision_map=NULL) {
 .ensure_class(spec,"audit_spec")
 if(!spec$lifecycle%in%c("EXPLORATORY","AMENDED")) .abort("Only pre-run exploratory/amended declarations can freeze","evaluably_error_not_frozen")
 if(isTRUE(attr(spec,"lineage_receipt")$has_run)) .abort("A run exists in this lineage; confirmation cannot be restored","evaluably_error_not_frozen")
 .require_verified(spec)
 if(!is.null(decision_map)&&!identical(decision_map$decision_map_hash,spec$decision_map_hash)) .abort("Decision map differs from declaration","evaluably_error_decision_map_hash_mismatch")
 for(r in spec$decision_map$rules) {
  e<-.rule_expression(r$when)
  check<-function(e) {
   if(is.call(e)) {
    if(identical(e[[1L]],as.name("$"))&&identical(e[[2L]],as.name("thresholds"))) {
     key<-as.character(e[[3L]]);if(!key%in%names(spec$thresholds)) .abort(paste("Undeclared threshold",key),"evaluably_error_undeclared_decision_rule")
    }
    for(z in as.list(e)) check(z)
   }
  };check(e)
 }
 .replace(spec,list(lifecycle="FROZEN_CONFIRMATORY",frozen_at=.now(),spec_hash=.hash(.spec_payload(spec))))
}
#' Record a Lineage-Preserving Amendment
#'
#' Records author, reason and parent hash. A scientific payload change cannot be labelled cosmetic. Any amendment after a known run makes the lineage irreversibly exploratory post hoc.
#' @param spec audit_spec object.
#' @param class Amendment classification; scientific changes cannot be labelled cosmetic.
#' @param description Written amendment reason.
#' @param actor Author of the amendment.
#' @param changes Named replacement declarations; derived/governance fields cannot be set.
#' @return Amended audit_spec; earlier objects and receipts are preserved.
#' @section Errors:
#' Malformed contracts raise structured MeasureQual conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(MeasureQual)
#' source(system.file("examples", "toy-workflow.R", package = "MeasureQual"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' amend_audit(t$spec, "SCIENTIFIC", "New declared endpoint", "analyst",
#'   list(endpoint_compatibility_declared = "UNDETERMINED"))
#' @export
amend_audit <- function(spec, class, description, actor, changes=list()) {
 if(inherits(spec,c("audit_run","audit_result"))) spec<-spec$spec
 .ensure_class(spec,"audit_spec");.require_verified(spec)
 .choice(class,c("NON_SCIENTIFIC","SCIENTIFIC","RESCUE_CONSIDERED","RESCUE_REFUSED","RESCUE_APPLIED","A1","A2","A3","A4"),"amendment class")
 .require_text(description,"description");.require_text(actor,"actor")
 if(!is.list(changes)||!.named(changes)) .abort("changes must be named")
 protected<-c("lifecycle","spec_hash","parent_spec_hash","amendment_ledger","frozen_at","audit_member_key","claim_contract_hash","metric_hash","metric_fn_hash")
 if(length(intersect(names(changes),protected))) .abort("Governance/derived fields cannot be amended directly")
 unknown<-setdiff(names(changes),names(spec));if(length(unknown)) .abort(paste("Unknown amendment fields",paste(unknown,collapse=", ")))
 x<-.replace(spec,changes)
 x<-.replace(x,.refresh_component_hashes(.plain(x))[c("representation_hash","representation_rule_hash","unit_spec_hash","reference_spec_hash","alternative_spec_hash","inferential_rule_hash","decision_map_hash")])
 x<-.replace(x,list(claim_contract_hash=.claim_contract_hash(x$claim,x$claim_class)))
 x<-.replace(x,list(audit_member_key=.audit_member_key(.member_components(x)),denominator_contract_hash=if(is.null(x$denominator_contract)) NULL else .hash(x$denominator_contract)))
 if(!is.null(x$denominator_contract)) .validate_denominator_contract(x$denominator_contract,x)
 changed<-!identical(.hash(.spec_payload(spec)),.hash(.spec_payload(x)))
 if(class%in%c("NON_SCIENTIFIC","A1","RESCUE_CONSIDERED","RESCUE_REFUSED")&&changed) .abort("Non-scientific amendment changes scientific payload")
 posthoc<-isTRUE(attr(spec,"lineage_receipt")$has_run)||spec$lifecycle=="EXPLORATORY_POST_HOC"
 state<-if(posthoc) "EXPLORATORY_POST_HOC" else if(changed) "AMENDED" else spec$lifecycle
 if(posthoc) .warn("Amendment after results: absorbing EXPLORATORY_POST_HOC","evaluably_warning_post_hoc_amendment")
 newhash<-.hash(.spec_payload(x));row<-list(parent_spec_hash=spec$spec_hash,new_spec_hash=newhash,class=class,timestamp=.now(),actor=actor,description=description)
 .replace(x,list(lifecycle=state,parent_spec_hash=if(changed) spec$spec_hash else spec$parent_spec_hash,spec_hash=newhash,
  amendment_ledger=c(spec$amendment_ledger,list(row)),flags=unique(c(x$flags,if(posthoc) "AMENDED_AFTER_RESULTS" else character())))) |> .rehash_spec()
}
.rehash_spec <- function(x) .replace(x,list(spec_hash=.hash(.spec_payload(x))))
