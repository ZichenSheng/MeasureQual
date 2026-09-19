.rule_context <- function(run) {
 spec<-run$spec;flags<-run$flags
 e<-as.list(setNames(.flags%in%flags,.flags))
 e<-c(e,list(alpha_reachable=run$census$inferential_support$alpha_reachable,support_gate=run$census$inferential_support$support_gate,
 capacity_gate=run$census$capacity$capacity_gate,unit_gate=run$census$unit_gate,endpoint_gate=run$census$endpoint_gate,
 equivalence_status=spec$representation_spec$equivalence_status,claim_class=spec$claim_class,
 direction_consistency_conditional=run$direction_consistency_conditional,direction_consistency=run$direction_consistency_conditional,
 recovery_fraction=if(is.null(run$recovery)) NA_real_ else run$recovery$detections/run$recovery$worlds,
 detections=if(is.null(run$recovery)) NA_integer_ else run$recovery$detections,worlds=spec$n_worlds,
 null_sd=run$null_dispersion$sd,stable_flag=if(nrow(run$detections)) all(run$detections$detected) else NA,
 detection_floor_bracketed=run$saturation$detection_floor_bracketed,frozen_failure_mode=run$frozen_failure_mode,
 calibration_value=run$calibration$value,calibration_outcome=run$calibration$outcome,thresholds=spec$thresholds))
 list2env(e,parent=baseenv())
}
#' Qualify One Declared Claim
#'
#' Applies non-overridable preflight outcomes or the first matching frozen author rule. No matching rule yields UNRESOLVED. Terminal states are unordered.
#' @param run audit_run, or blocked audit_check for qualification.
#' @param decision_map Explicit decision map declaration; no scientific value is inferred by the engine.
#' @param census Constructibility receipt bound to this spec and severity grid.
#' @param integrity Integrity receipt; cannot replace a conflicting executed receipt.
#' @return audit_result with state, flags and rule provenance; no interpretation prose.
#' @section Errors:
#' Malformed contracts raise structured MeasureQual conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(MeasureQual)
#' source(system.file("examples", "toy-workflow.R", package = "MeasureQual"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' apply_decision_map(run_audit(check_audit(s, t$data), t$data))
#' @export
apply_decision_map <- function(run,decision_map=NULL,census=NULL,integrity=NULL) {
 if(!inherits(run,c("audit_run","audit_check"))) .abort("Qualification requires run or blocked check")
 .require_verified(run);spec<-run$spec
 if(is.null(decision_map)) decision_map<-spec$decision_map
 .ensure_class(decision_map,"decision_map")
 if(!identical(decision_map$decision_map_hash,spec$decision_map_hash)||!identical(.component_hash(decision_map,"decision_map_hash"),spec$decision_map_hash)) .abort("Frozen decision map mismatch","evaluably_error_decision_map_hash_mismatch")
 if(!is.null(census)&&!identical(.hash(.plain(census)),.hash(.plain(run$census)))) .abort("Census does not belong to this execution","evaluably_error_freeze_verification")
 if(!is.null(integrity)&&!identical(.hash(integrity),.hash(run$integrity))) .abort("Integrity receipt mismatch","evaluably_error_freeze_verification")
 state<-"UNRESOLVED";rule_id<-NA_character_;because<-"NO_MATCHING_RULE";precedence<-"P4"
 flags<-run$flags
 if(inherits(run,"audit_check")) {
  if(run$entry_gate!="BLOCKED") .abort("A passing check must be executed before qualification")
  state<-run$preterminal_state;because<-run$stop_reason
  precedence<-switch(run$blocking_gate,integrity="P1",capacity_gate="P2",support_gate="P2b",endpoint_gate="P2c",unit_gate="P2d")
  eq<-spec$representation_spec$equivalence_status
  flags<-unique(c(flags,spec$flags,spec$unit_spec$flags,if(eq=="NOT_EQUIVALENT") "REPRESENTATION_NOT_EQUIVALENT" else if(eq=="UNKNOWN") "REPRESENTATION_EQUIVALENCE_UNKNOWN" else character()))
 } else {
  env<-.rule_context(run)
  for(r in decision_map$rules) {
   value<-tryCatch(eval(.rule_expression(r$when),env),error=function(e) .abort(conditionMessage(e),"evaluably_error_undeclared_decision_rule"))
   if(!is.logical(value)||length(value)!=1L) .abort("Decision predicate must return a scalar logical")
   if(isTRUE(value)) {state<-r$then;rule_id<-r$id;because<-r$because;precedence<-"P3";break}
  }
 }
 descriptor<-NULL
 if(spec$claim_class=="MEASUREMENT_CAPABILITY"&&!state%in%c("NOT_EVALUABLE","INVALID")) descriptor<-if(state=="SUPPORTED_WITHIN_DOMAIN") "CLAIM_DETECTABLE" else "NOT_DEMONSTRATED"
 .choice(state,.terminal_states,"terminal_state")
 x<-list(spec=spec,terminal_state=state,flags=unique(flags),capability_descriptor=descriptor,
 rule_id=rule_id,because=because,precedence_rule_fired=precedence,decision_map_hash=spec$decision_map_hash,
 run_hash=run$run_hash,lifecycle=spec$lifecycle,descriptors=list(frozen_failure_mode=run$frozen_failure_mode))
 x$result_hash<-.hash(.plain(x)[setdiff(names(x),"spec")]);structure(x,class="audit_result")
}
.license <- function(spec,result,execution,unit) {
 state<-as.character(result$terminal_state);flags<-result$flags
 reasons<-character()
 if(spec$lifecycle!="FROZEN_CONFIRMATORY") reasons<-c(reasons,"LIFECYCLE_NOT_CONFIRMATORY")
 reasons<-c(reasons,intersect(flags,c("FREEZE_VERIFICATION_FAILED","HUMAN_REVIEW_REQUIRED","ALPHA_UNREACHABLE","FIREWALL_INACTIVE","UNIT_MISMATCH_DECLARED")))
 if(state%in%c("INVALID","NOT_EVALUABLE")) reasons<-c(reasons,state)
 disposition<-if(length(reasons)) "SUPPRESSED" else if(state=="SUPPORTED_WITHIN_DOMAIN") "LICENSED" else "NOT_LICENSED"
 qualifiers<-if(disposition=="LICENSED") c("DOMAIN_BOUND","REPRESENTATION_BOUND","DESIGN_BOUND") else character()
 map<-c(PANEL_BOUNDED="PANEL_BOUND",LOW_SCIENTIFIC_N="LOW_SCIENTIFIC_N",REFERENCE_DEPENDENT="REFERENCE_DEPENDENT",SATURATED="SATURATION_LIMITED",DETECTION_FLOOR_NOT_BRACKETED="SATURATION_LIMITED",REPRESENTATION_EQUIVALENCE_UNKNOWN="EQUIVALENCE_UNKNOWN")
 qualifiers<-c(qualifiers,unname(map[intersect(names(map),flags)]))
 if(spec$claim_class=="MEASUREMENT_CAPABILITY") qualifiers<-c(qualifiers,"CAPABILITY_ONLY")
 if(identical(execution$calibration$calibration_scope,"DESIGN_LEVEL")) qualifiers<-c(qualifiers,"CALIBRATION_SCOPE_DESIGN_LEVEL")
 prohibitions<-c("CROSS_CONTEXT_TRANSFER","CROSS_REPRESENTATION_TRANSFER","UNTESTED_DOMAIN_EXTENSION")
 if(spec$representation_spec$equivalence_status%in%c("NOT_EQUIVALENT","UNKNOWN")) prohibitions<-c(prohibitions,"ORIGINAL_MODEL_EQUIVALENCE")
 if(spec$claim_class!="DIRECT_IDENTIFICATION") prohibitions<-c(prohibitions,"UNIQUE_IDENTIFICATION")
 if(spec$claim_class=="MEASUREMENT_CAPABILITY") prohibitions<-c(prohibitions,"BIOLOGICAL_TRUTH")
 if(state%in%c("UNRESOLVED","NOT_EVALUABLE")) prohibitions<-c(prohibitions,"BIOLOGICAL_ABSENCE")
 if(any(c("ALPHA_UNREACHABLE","ZERO_CAPACITY_PRESENT")%in%flags)) prohibitions<-c(prohibitions,"SENSITIVITY_OR_POWER_STATEMENT")
 list(declared_claim_id=spec$claim$claim_id,disposition=disposition,
 licensed_scope=list(representation_id=spec$representation_spec$representation_id,representation_hash=spec$representation_hash,
  context=spec$context$context_id,inferential_design_id=substr(spec$unit_spec_hash,1,12),
  severity_levels_executed=if(isTRUE(execution$executed)) spec$severity_grid else numeric(),scientific_unit=spec$unit_spec$scientific,n_scientific=unit$n_scientific),
 required_qualifiers=unique(qualifiers),prohibited_extensions=unique(prohibitions),suppression=list(suppressed=length(reasons)>0L,reasons=unique(reasons)))
}
