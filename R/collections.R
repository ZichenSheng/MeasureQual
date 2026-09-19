.record_comparability <- function(r,severity) {
 c(list(claim_contract_hash=r$identity$claim_contract_hash,metric_hash=r$declarations$metric_hash,context_id=r$identity$context_id),
 r$declarations[c("unit_spec_hash","reference_spec_hash","alternative_spec_hash","inferential_rule_hash","decision_map_hash")],list(severity_level=severity),r$declarations["representation_rule_hash"])
}
#' Collect Records for Side-by-Side Reporting
#'
#' Computes comparability using the ten frozen components. Collections carry no ranking, pooling or quality summary. Differing metric versions require an explicit incomparable-display declaration.
#' @param records List of independent evidence_record objects.
#' @param collection_reason Nonempty reason for side-by-side display.
#' @param comparison_contract Explicit ten-component homogeneous-comparison contract, or NULL.
#' @param prespecified_total Optional user-declared family size; does not itself prove chronology.
#' @param allow_incomparable Explicit permission to display differing metric versions.
#' @return Reporting-only audit_collection.
#' @section Errors:
#' Malformed contracts raise structured MeasureQual conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(MeasureQual)
#' source(system.file("examples", "toy-workflow.R", package = "MeasureQual"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' r <- evidence_record(run_audit(check_audit(s, t$data), t$data))
#' audit_collection(list(r), r$identity$audit_member_key)
#' @export
audit_collection <- function(records,collection_reason,comparison_contract=NULL,prespecified_total=NULL,allow_incomparable=FALSE) {
 .require_text(collection_reason,"collection_reason")
 if(!is.list(records)||!length(records)||!all(vapply(records,inherits,logical(1),"evidence_record"))) .abort("records must contain evidence_record objects")
 for(r in records) .require_verified(r)
 versions<-unique(vapply(records,function(r) r$declarations$metric_version,character(1)))
 if(length(versions)>1L&&!isTRUE(allow_incomparable)) .abort("Metric versions differ; allow_incomparable must be explicit","evaluably_error_incomparable_collection")
 if(!is.null(prespecified_total)) prespecified_total<-.int(prespecified_total,"prespecified_total")
 allowed<-FALSE
 if(!is.null(comparison_contract)) {
  if(!is.list(comparison_contract)||!setequal(names(comparison_contract),.comparison_fields)) .abort("comparison_contract must contain the ten frozen components")
  allowed<-all(vapply(records,function(r) comparison_contract$severity_level%in%r$declarations$severity_grid&&identical(.hash(.record_comparability(r,comparison_contract$severity_level)),.hash(comparison_contract)),logical(1)))
 }
 subjects<-unique(vapply(records,function(r) r$identity$subject_id,character(1)))
 if(length(subjects)==1L) {
  keys<-vapply(records,function(r) .hash(list(claim_family=r$identity$claim_family,context=r$identity$context_id,design=r$declarations$inferential_rule_hash)),character(1))
  if(length(unique(keys))>1L) allowed<-FALSE
 }
 structure(list(records=records,collection_reason=collection_reason,comparison_contract=comparison_contract,prespecified_total=prespecified_total,
 aggregation_allowed=allowed,denominator_frozen=NA_character_,flags=c("NOT_A_SIGNATURE_SUMMARY",if(length(versions)>1L) "INCOMPARABLE_METRICS" else character())),class="audit_collection")
}
#' Report a Declared Homogeneous Denominator
#'
#' Rejects duplicate prospective members and incomparable records. Returns labelled integer counts with CONTRACT_VERIFIED or DECLARED_ONLY provenance. No proportion, percentage or pass rate is returned.
#' @param collection audit_collection; counts require recomputed comparability.
#' @return Plain list of denominator counts, missing/unexpected keys and provenance sentence.
#' @section Errors:
#' Malformed contracts raise structured MeasureQual conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(MeasureQual)
#' source(system.file("examples", "toy-workflow.R", package = "MeasureQual"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' r <- evidence_record(run_audit(check_audit(s, t$data), t$data))
#' fields <- c("claim_contract_hash", "metric_hash", "unit_spec_hash",
#'   "reference_spec_hash", "alternative_spec_hash", "inferential_rule_hash",
#'   "decision_map_hash", "representation_rule_hash")
#' contract <- s[fields]
#' contract$context_id <- s$context$context_id
#' contract$severity_level <- 1
#' c <- audit_collection(list(r), "One declared member", contract)
#' denominator_summary(c)
#' @export
denominator_summary <- function(collection) {
 .ensure_class(collection,"audit_collection");rs<-collection$records
 for(r in rs) .require_verified(r)
 keys<-vapply(rs,function(r) r$identity$audit_member_key,character(1))
 if(anyDuplicated(keys)) {
  key<-keys[duplicated(keys)][1];ids<-vapply(rs[keys==key],function(r) r$identity$record_id,character(1))
  .abort(paste("Duplicate member",key,"records",paste(ids,collapse=", ")),"evaluably_error_duplicate_member_key")
 }
 # Recompute instead of trusting a mutable reporting flag.
 validated<-audit_collection(rs,collection$collection_reason,collection$comparison_contract,collection$prespecified_total,allow_incomparable=TRUE)
 if(!validated$aggregation_allowed) .abort("Incomparable records cannot be summarized","evaluably_error_incomparable_collection")
 contracts<-lapply(rs,function(r) r$declarations$denominator_contract)
 present<-!vapply(contracts,is.null,logical(1));shared<-all(present)&&length(unique(vapply(contracts,.hash,character(1))))==1L
 expected<-unique(unlist(lapply(contracts,function(d) d$expected_member_keys),use.names=FALSE))
 unexpected<-setdiff(keys,expected);missing_keys<-setdiff(expected,keys)
 verified<-shared&&!length(unexpected)&&all(vapply(rs,function(r) identical(r$declarations$denominator_contract_hash,.hash(r$declarations$denominator_contract)),logical(1)))
 if(verified) {
  contract<-contracts[[1L]]
  verified<-contract$prespecified_total==length(contract$expected_member_keys)&&!anyDuplicated(contract$expected_member_keys)
 }
 total<-if(verified) contracts[[1L]]$prespecified_total else if(is.null(collection$prespecified_total)) NA_integer_ else collection$prespecified_total
 entry<-vapply(rs,function(r) r$preflight$entry_gate=="PASS",logical(1))
 failure<-vapply(rs[!entry],function(r) r$preflight$stop_reason,character(1))
 outcomes<-vapply(rs[entry],function(r) as.character(r$qualification$terminal_state),character(1))
 failure_counts<-table(factor(failure,levels=.stop_reasons));outcome_counts<-table(factor(outcomes,levels=.terminal_states))
 provenance<-if(verified) "CONTRACT_VERIFIED" else "DECLARED_ONLY"
 sentence<-if(verified) sprintf("%s units were prespecified under contract %s, frozen at %s and hash-verified against every record; %s passed all entry gates.",total,contracts[[1L]]$contract_id,contracts[[1L]]$frozen_at,sum(entry)) else sprintf("%s units were reported as prespecified. This chronology is user-declared and is NOT verified by this software; no pre-run contract is bound to these records. %s passed all entry gates.",total,sum(entry))
 if(length(missing_keys)||length(unexpected)) sentence<-paste("[WARNING: denominator membership is incomplete or unexpected]",sentence)
 list(prespecified_total=total,entry_qualified_total=sum(entry),entry_failures_by_reason=setNames(as.integer(failure_counts),names(failure_counts)),
 outcome_counts=setNames(as.integer(outcome_counts),names(outcome_counts)),comparison_contract=collection$comparison_contract,
 denominator_provenance=provenance,unaccounted_expected_member_keys=missing_keys,unexpected_member_keys=unexpected,sentence=sentence)
}
#' @export
as.data.frame.audit_collection <- function(x,...) {
 do.call(rbind,lapply(x$records,function(r) data.frame(record_id=r$identity$record_id,subject_id=r$identity$subject_id,
 claim_family=r$identity$claim_family,claim_class=r$identity$claim_class,context=r$identity$context_id,
 representation_hash=r$declarations$representation_hash,representation_rule_hash=r$declarations$representation_rule_hash,
 terminal_state=as.character(r$qualification$terminal_state),comparability=if(x$aggregation_allowed) "COMPARABLE" else "INCOMPARABLE",
 aggregation_allowed=x$aggregation_allowed,stringsAsFactors=FALSE)))
}
