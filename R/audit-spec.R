#' Bind the Claim-Scoped Scientific Contract
#'
#' Binds subject, representation, claim, context and inferential design. All scientific choices remain explicit. Callback identity, prospective membership and optional denominator contracts are computed before freezing.
#' @param subject List with subject_id, subject_label and subject_provenance.
#' @param claim List with claim_id, claim_family, text, estimand and scope.
#' @param claim_class Frozen claim-class vocabulary member.
#' @param context List containing context_id and description.
#' @param endpoint_compatibility_declared SUPPORTED, INCOMPATIBLE or UNDETERMINED; the package does not judge endpoint adequacy.
#' @param metric_fn Callback returning a finite numeric vector from a world.
#' @param metric_direction Explicit greater/less direction (legacy higher_better/lower_better accepted).
#' @param severity_grid Strictly increasing nonnegative declared severities; zero requires null_generator.
#' @param representation_spec Explicit representation spec declaration; no scientific value is inferred by the engine.
#' @param unit_spec Explicit unit spec declaration; no scientific value is inferred by the engine.
#' @param reference_spec Explicit reference spec declaration; no scientific value is inferred by the engine.
#' @param alternative_spec Explicit alternative spec declaration; no scientific value is inferred by the engine.
#' @param inferential_rule Explicit inferential rule declaration; no scientific value is inferred by the engine.
#' @param decision_map Explicit decision map declaration; no scientific value is inferred by the engine.
#' @param n_worlds Number of scheduled worlds per severity.
#' @param metric_version Explicit metric version declaration; no scientific value is inferred by the engine.
#' @param metric_fn_depends_on Explicit nested callback dependency declaration: packages, constants and functions. Helpers use list(fn, depends_on); raw helper hashes are forbidden.
#' @param null_generator Explicit null-world callback for severity zero.
#' @param null_generator_depends_on Explicit nested callback dependency declaration: packages, constants and functions. Helpers use list(fn, depends_on); raw helper hashes are forbidden.
#' @param min_constructibility Explicit min constructibility declaration; no scientific value is inferred by the engine.
#' @param detection_target Explicit detection target declaration; no scientific value is inferred by the engine.
#' @param stable_flag_rule Explicit stable flag rule declaration; no scientific value is inferred by the engine.
#' @param stable_flag_rule_depends_on Explicit nested callback dependency declaration: packages, constants and functions. Helpers use list(fn, depends_on); raw helper hashes are forbidden.
#' @param thresholds Explicit thresholds declaration; no scientific value is inferred by the engine.
#' @param pairing Explicit pairing declaration; no scientific value is inferred by the engine.
#' @param replacement_policy Explicit replacement policy declaration; no scientific value is inferred by the engine.
#' @param max_replacements Explicit max replacements declaration; no scientific value is inferred by the engine.
#' @param replacement_fn Explicit replacement fn declaration; no scientific value is inferred by the engine.
#' @param replacement_fn_depends_on Explicit nested callback dependency declaration: packages, constants and functions. Helpers use list(fn, depends_on); raw helper hashes are forbidden.
#' @param denominator_contract Explicit denominator contract declaration; no scientific value is inferred by the engine.
#' @param notes Explicit notes declaration; no scientific value is inferred by the engine.
#' @param source_locators Explicit source locators declaration; no scientific value is inferred by the engine.
#' @param source_sha256 Explicit source sha256 declaration; no scientific value is inferred by the engine.
#' @param inner_resamples Explicit inner resamples declaration; no scientific value is inferred by the engine.
#' @param dataset_settings Explicit dataset settings declaration; no scientific value is inferred by the engine.
#' @param scientific_replicates Explicit scientific replicates declaration; no scientific value is inferred by the engine.
#' @param plateau_tol Explicit plateau tol declaration; no scientific value is inferred by the engine.
#' @param plateau_min_rungs Explicit plateau min rungs declaration; no scientific value is inferred by the engine.
#' @param ceiling_tol Explicit ceiling tol declaration; no scientific value is inferred by the engine.
#' @param theoretical_max Explicit theoretical max declaration; no scientific value is inferred by the engine.
#' @param calibration Explicit calibration declaration; no scientific value is inferred by the engine.
#' @param frozen_failure_mode Explicit frozen failure mode declaration; no scientific value is inferred by the engine.
#' @param flags Explicit flags declaration; no scientific value is inferred by the engine.
#' @param audit_id Explicit audit id declaration; no scientific value is inferred by the engine.
#' @param construct Explicit construct declaration; no scientific value is inferred by the engine.
#' @return Exploratory audit_spec ready for freeze_audit.
#' @section Errors:
#' Malformed contracts raise structured evaluably conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(evaluably)
#' source(system.file("examples", "toy-workflow.R", package = "evaluably"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' t$spec
#' @export
audit_spec <- function(subject, claim, claim_class, context, endpoint_compatibility_declared,
 metric_fn, metric_direction, severity_grid, representation_spec, unit_spec, reference_spec,
 alternative_spec, inferential_rule, decision_map, n_worlds, metric_version="1.0.0",
 metric_fn_depends_on=NULL, null_generator=NULL, null_generator_depends_on=NULL,
 min_constructibility=NULL, detection_target=NULL, stable_flag_rule=NULL,
 stable_flag_rule_depends_on=NULL, thresholds=list(), pairing="unpaired",
 replacement_policy="none", max_replacements=0L, replacement_fn=NULL, replacement_fn_depends_on=NULL,
 denominator_contract=NULL, notes=NULL, source_locators=list(), source_sha256=character(),
 inner_resamples=1L, dataset_settings=1L, scientific_replicates=NULL,
 plateau_tol=NULL, plateau_min_rungs=NULL, ceiling_tol=NULL, theoretical_max=NULL,
 calibration=NULL, frozen_failure_mode=NULL, flags=character(), audit_id=NULL, construct=NULL) {
 for(n in c("subject_id","subject_provenance")) .require_text(subject[[n]],n)
 for(n in c("claim_id","claim_family","text","estimand","scope")) .require_text(claim[[n]],n)
 for(n in c("context_id","description")) .require_text(context[[n]],n)
 ch<-.claim_contract_hash(claim,claim_class)
 .choice(endpoint_compatibility_declared,c("SUPPORTED","INCOMPATIBLE","UNDETERMINED"),"endpoint_compatibility_declared")
 .choice(metric_direction,c("greater","less","higher_better","lower_better"),"metric_direction")
 .require_text(metric_version,"metric_version")
 components<-list(representation_spec=representation_spec,unit_spec=unit_spec,reference_spec=reference_spec,
 alternative_spec=alternative_spec,inferential_rule=inferential_rule,decision_map=decision_map)
 for(n in names(components)) { .ensure_class(components[[n]],n);.validate_callbacks(components[[n]]) }
 if(!is.numeric(severity_grid)||!length(severity_grid)||any(!is.finite(severity_grid))||any(severity_grid<0)||any(diff(severity_grid)<=0)) .abort("severity_grid must be finite, nonnegative and strictly increasing")
 if(0%in%severity_grid&&is.null(null_generator)) .abort("A null rung requires an explicitly declared null_generator")
 n_worlds<-.int(n_worlds,"n_worlds");inner_resamples<-.int(inner_resamples,"inner_resamples");dataset_settings<-.int(dataset_settings,"dataset_settings")
 if(!is.null(scientific_replicates)) scientific_replicates<-.int(scientific_replicates,"scientific_replicates")
 if(!is.null(min_constructibility)) .scalar_number(min_constructibility,0,1,"min_constructibility")
 if(!is.null(detection_target)) .scalar_number(detection_target,0,1,"detection_target")
 .choice(pairing,c("paired","unpaired"),"pairing")
 .choice(replacement_policy,c("none","scheduled_only","declared"),"replacement_policy")
 max_replacements<-.int(max_replacements,"max_replacements",0)
 if(replacement_policy=="declared"&&!is.function(replacement_fn)) .abort("Declared replacement policy requires replacement_fn")
 if(!is.list(thresholds)||!.named(thresholds)) .abort("thresholds must be a named list")
 .strings(flags,"flags");if(any(!flags%in%.flags)) .abort("Unknown software flag")
 if(!is.null(frozen_failure_mode)) .choice(frozen_failure_mode,c("NULL_GEOMETRY_LIMITED","SHIFT_LIMITED","INDETERMINATE"),"frozen_failure_mode")
 if(!is.null(calibration)) {
  if(!is.list(calibration)||!all(c("value","criterion","outcome","calibration_scope")%in%names(calibration))) .abort("Incomplete calibration declaration")
  .choice(calibration$calibration_scope,c("DESIGN_LEVEL","UNIT_SPECIFIC"),"calibration_scope")
 }
 for(n in c("plateau_tol","ceiling_tol")) if(!is.null(get(n))) .scalar_number(get(n),0,Inf,n)
 if(!is.null(plateau_min_rungs)) plateau_min_rungs<-.int(plateau_min_rungs,"plateau_min_rungs",2)
 if(!is.null(theoretical_max)&&(!is.numeric(theoretical_max)||length(theoretical_max)!=1L||!is.finite(theoretical_max))) .abort("Invalid theoretical_max")
 if(length(source_sha256)&&!all(vapply(source_sha256,.sha_valid,logical(1)))) .abort("Invalid archival source SHA256")
 cb<-.cb(metric_fn,metric_fn_depends_on,"metric_fn")
 callbacks<-list(metric_fn=metric_fn,null_generator=null_generator,stable_flag_rule=stable_flag_rule,replacement_fn=replacement_fn)
 dependencies<-list(metric_fn=metric_fn_depends_on,null_generator=null_generator_depends_on,stable_flag_rule=stable_flag_rule_depends_on,replacement_fn=replacement_fn_depends_on)
 x<-c(list(subject=subject,claim=claim,claim_class=claim_class,context=context,endpoint_compatibility_declared=endpoint_compatibility_declared,
 metric_hash=cb$digest,metric_version=metric_version,metric_direction=metric_direction,severity_grid=severity_grid),components)
 for(n in names(callbacks)) { cbi<-.cb(callbacks[[n]],dependencies[[n]],n);x[paste0(n,"_hash")]<-list(cbi$digest);x[paste0(n,"_depends_on")]<-list(cbi$manifest) }
 x<-c(x,list(n_worlds=n_worlds,inner_resamples=inner_resamples,dataset_settings=dataset_settings,
 scientific_replicates=scientific_replicates,min_constructibility=min_constructibility,detection_target=detection_target,
 thresholds=thresholds,pairing=pairing,replacement_policy=replacement_policy,max_replacements=max_replacements,
 plateau_tol=plateau_tol,plateau_min_rungs=plateau_min_rungs,ceiling_tol=ceiling_tol,theoretical_max=theoretical_max,
 calibration=calibration,frozen_failure_mode=frozen_failure_mode,flags=flags,source_locators=source_locators,source_sha256=source_sha256,
 audit_id=audit_id,construct=construct,notes=notes,claim_contract_hash=ch))
 x<-.refresh_component_hashes(x)
 x$audit_member_key<-.audit_member_key(.member_components(x))
 x["denominator_contract"]<-list(denominator_contract);x["denominator_contract_hash"]<-list(NULL)
 if(!is.null(denominator_contract)) {
  .validate_denominator_contract(denominator_contract,x)
  x$denominator_contract_hash<-.hash(denominator_contract)
 }
 x$lifecycle<-"EXPLORATORY";x$amendment_ledger<-list();x$parent_spec_hash<-NULL;x$spec_hash<-NULL;x$frozen_at<-NULL
 x<-.attach_callbacks(structure(x,class="audit_spec"),callbacks,dependencies)
 receipt<-new.env(parent=emptyenv());receipt$has_run<-FALSE;receipt$history<-list()
 attr(x,"lineage_receipt")<-receipt
 .forbidden_keys(x);x
}
.refresh_component_hashes <- function(x) {
 x$representation_hash<-x$representation_spec$representation_hash
 x$representation_rule_hash<-x$representation_spec$representation_rule_hash
 for(n in c("unit_spec","reference_spec","alternative_spec","inferential_rule","decision_map")) x[[paste0(n,"_hash")]]<-x[[n]][[paste0(n,"_hash")]]
 x
}
.member_components <- function(x) c(list(subject_id=x$subject$subject_id,context_id=x$context$context_id),x[setdiff(.member_fields,c("subject_id","context_id"))])
.comparability <- function(x,severity_level) c(x[c("claim_contract_hash","metric_hash")],list(context_id=x$context$context_id),x[c("unit_spec_hash","reference_spec_hash","alternative_spec_hash","inferential_rule_hash","decision_map_hash")],list(severity_level=severity_level),x["representation_rule_hash"])
.validate_denominator_contract <- function(d,x) {
 required<-c("contract_id","claim_contract_hash","expected_member_keys","prespecified_total","comparability","frozen_at")
 if(!is.list(d)||!setequal(names(d),required)) .abort("Invalid denominator_contract fields")
 .require_text(d$contract_id,"contract_id");.require_text(d$frozen_at,"frozen_at")
 if(is.na(as.POSIXct(d$frozen_at,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC"))) .abort("Invalid denominator frozen_at")
 k<-d$expected_member_keys
 if(!is.character(k)||length(k)<2L||anyDuplicated(k)||!all(vapply(k,.sha_valid,logical(1)))||!identical(k,sort(k,method="radix"))) .abort("Expected member keys must be sorted, unique SHA256 values")
 if(!identical(as.integer(d$prespecified_total),as.integer(length(k)))||!x$audit_member_key%in%k) .abort("Denominator size or own member key mismatch")
 if(!identical(d$claim_contract_hash,x$claim_contract_hash)) .abort("Claim contract mismatch")
 if(!setequal(names(d$comparability),.comparison_fields)||!identical(.hash(d$comparability),.hash(.comparability(x,d$comparability$severity_level)))) .abort("Denominator comparability mismatch")
 if(!d$comparability$severity_level%in%x$severity_grid) .abort("Denominator severity is not in declared grid")
 invisible(TRUE)
}
