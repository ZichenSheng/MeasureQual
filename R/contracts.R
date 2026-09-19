#' @importFrom stats setNames
#' @importFrom utils head tail
NULL
.s3_classes <- c("representation_spec", "unit_spec", "reference_spec", "alternative_spec",
 "inferential_rule", "decision_map", "audit_spec", "audit_check", "constructibility_census",
 "audit_run", "audit_result", "evidence_record", "audit_collection")
.terminal_states <- c("SUPPORTED_WITHIN_DOMAIN", "DEFINITION_IMPLIED", "DIRECTIONALLY_MISLEADING", "NOT_EVALUABLE", "INVALID", "UNRESOLVED")
.flags <- c("REFERENCE_DEPENDENT", "DEGENERATE_NULL", "SATURATED", "DETECTION_FLOOR_NOT_BRACKETED", "LOW_CONSTRUCTIBILITY", "ZERO_CAPACITY_PRESENT", "DOMAIN_BOUNDED", "PANEL_BOUNDED", "LOW_SCIENTIFIC_N", "UNIT_MISMATCH_DECLARED", "WORLD_AS_SCIENTIFIC_UNIT", "UNBALANCED_BLOCKS", "CROSSED_BLOCKING", "NO_REFERENCE", "FIREWALL_INACTIVE", "DECLARED_TRUTH_DEPENDENCE", "REPLACEMENT_USED", "AMENDED_AFTER_RESULTS", "HUMAN_REVIEW_REQUIRED", "INCOMPARABLE_METRICS", "FREEZE_VERIFICATION_FAILED", "ALPHA_UNREACHABLE", "REPRESENTATION_NOT_EQUIVALENT", "REPRESENTATION_EQUIVALENCE_UNKNOWN")
.stop_reasons <- c("ZERO_CAPACITY", "ALPHA_UNREACHABLE", "ENDPOINT_INCOMPATIBLE", "NO_LEGAL_UNIT", "INVALID_TRUTH_LEAKAGE", "HUMAN_REVIEW")
.comparison_fields <- c("claim_contract_hash", "metric_hash", "context_id", "unit_spec_hash", "reference_spec_hash", "alternative_spec_hash", "inferential_rule_hash", "decision_map_hash", "severity_level", "representation_rule_hash")
.warn <- function(message, class) rlang::warn(message, class = c(class, "evaluably_condition"), code = class, field = NULL, observed = NULL, expected = NULL)
.now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
.plain <- function(x) {
 if (is.data.frame(x)) return(x)
 if (is.list(x)) { y <- lapply(x, .plain); names(y) <- names(x); return(y) }
 if (is.factor(x)) return(as.character(x))
 x
}
.replace <- function(x, values) {
 at <- attributes(x); z <- unclass(x)
 for(n in names(values)) z[n] <- values[n]
 for(n in setdiff(names(at), c("names", "class"))) attr(z,n) <- at[[n]]
 class(z) <- class(x); z
}

.payload <- function(x) {
 y <- .plain(x)
 if (inherits(x,"reference_spec")) y$source <- NULL
 y
}
.cb <- function(f, d, role) if (is.null(f)) NULL else .resolve_callback(f,d,role)
.attach_callbacks <- function(x, callbacks, declarations) {
 attr(x,"callbacks") <- callbacks; attr(x,"callback_declarations") <- declarations; x
}
.validate_callbacks <- function(x) {
 # Re-resolve current closures: environments never enter a hash.
 cb <- attr(x,"callbacks"); dd <- attr(x,"callback_declarations")
 for(n in names(cb)) if(!is.null(cb[[n]])) {
  observed <- .callback_digest(cb[[n]],dd[[n]],n)
  expected <- x[[paste0(n,"_hash")]]
  if (!identical(observed,expected)) .abort(paste("Callback changed:",n),"evaluably_error_freeze_verification")
 }
 if(inherits(x,"representation_spec") && !is.null(attr(x,"runtime_score_fn"))) {
  observed <- .callback_digest(attr(x,"runtime_score_fn"),attr(x,"score_declaration"),"score_fn")
  if(!identical(observed,x$score_fn_hash)) .abort("score_fn changed","evaluably_error_freeze_verification")
 }
 if(inherits(x,"inferential_rule") && !is.null(attr(x,"runtime_support_fn"))) {
  observed <- .callback_digest(attr(x,"runtime_support_fn"),attr(x,"support_declaration"),"support_fn")
  if(!identical(observed,x$support_fn_hash)) .abort("support_fn changed","evaluably_error_freeze_verification")
 }
 if(inherits(x,"alternative_spec")) {
  cs <- attr(x,"constraint_callbacks"); ds <- attr(x,"constraint_declarations")
  for(n in names(cs)) {
   observed <- .resolve_callback(cs[[n]],ds[[n]],paste0("constraint_",n))$manifest
   if(!identical(observed,x$constraints[[n]])) .abort("Constraint callback changed","evaluably_error_freeze_verification")
  }
 }
 invisible(TRUE)
}
.int <- function(x, field, lower=1) {
 if(!is.numeric(x)||length(x)!=1L||!is.finite(x)||x!=floor(x)||x<lower) .abort(paste("Invalid integer",field))
 as.integer(x)
}
.strings <- function(x, field) if(!is.character(x)||anyNA(x)||any(!nzchar(x))||anyDuplicated(x)) .abort(paste("Invalid",field))
.ensure_class <- function(x, cls) if(!inherits(x,cls)) .abort(paste("Expected",cls))
.component_hash <- function(x, field) .hash(.payload(x)[setdiff(names(.payload(x)),field)])

.select <- function(x, fields) setNames(lapply(fields,function(n) x[[n]]),fields)

.inform <- function(message, class) rlang::inform(message, class = c(class, "evaluably_condition"), code = class, field = NULL, observed = NULL, expected = NULL)
