# Frozen V1 section 23 taxonomy with the V1.0.1/1.0.2/1.0.2A deltas.
.condition_classes <- c("evaluably_error_invalid_spec",
  "evaluably_error_feature_coverage",
  "evaluably_error_unsupported_representation_relation",
  "evaluably_error_illegal_unit",
  "evaluably_error_reachability_undetermined",
  "evaluably_error_not_frozen",
  "evaluably_error_freeze_verification",
  "evaluably_error_decision_map_hash_mismatch",
  "evaluably_error_severity_grid_mismatch",
  "evaluably_error_truth_leakage",
  "evaluably_error_entry_gate_blocked",
  "evaluably_error_incomparable_collection",
  "evidence_forbidden_inheritance_key",
  "evaluably_error_undeclared_decision_rule",
  "evaluably_warning_nondeterministic_generator",
  "evaluably_warning_post_hoc_amendment",
  "evaluably_warning_unit_mismatch_declared",
  "evaluably_warning_declared_truth_dependence",
  "evaluably_warning_unreachable_rule",
  "evaluably_message_feature_coverage",
  "evaluably_message_degenerate_null",
  "evaluably_error_undeclared_callback_dependency",
  "evaluably_error_callback_dependency_value_mismatch",
  "evaluably_warning_unused_callback_dependency",
  "evaluably_error_unserializable_data",
  "evaluably_error_data_fingerprint_mismatch",
  "evaluably_error_duplicate_member_key",
  "evaluably_error_callback_recursion_depth",
  "evaluably_error_callback_dependency_cycle")

.abort <- function(message, class = "evaluably_error_invalid_spec") {
  rlang::abort(message, class = c(class, "evaluably_condition"), code = class, field = NULL, observed = NULL, expected = NULL)
}
.text <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
.choice <- function(x, choices, field) {
  if (!.text(x) || !x %in% choices) .abort(paste(field, "must be one of", paste(choices, collapse = ", ")))
}
.scalar_number <- function(x, lower, upper, field, open = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      if (open) x <= lower || x >= upper else x < lower || x > upper)
    .abort(paste("Invalid", field))
}
.sha_valid <- function(x) .text(x) && grepl("^[a-f0-9]{64}$", x)
.require_text <- function(x, field) if (!.text(x)) .abort(paste(field, "must be nonempty text"))
.named <- function(x) length(x) == 0L || (!is.null(names(x)) &&
  !anyNA(names(x)) && all(nzchar(names(x))) && !anyDuplicated(names(x)))

# Scans structure only: never follows or hashes environments.
.forbidden_keys <- function(x) {
  if ("inherits_evidence_from" %in% names(x))
    .abort("inherits_evidence_from is forbidden at every nesting depth", "evidence_forbidden_inheritance_key")
  if (is.list(x) || is.pairlist(x)) for (v in x) .forbidden_keys(v)
  invisible(TRUE)
}
.canonical_tree <- function(x) {
  if (is.environment(x) || is.function(x) || typeof(x) %in% c("externalptr", "weakref"))
    .abort("Runtime objects cannot enter canonical scientific serialization")
  if (is.data.frame(x)) {
    # Preserve row order (positional); normalize object/column names only.
    return(lapply(seq_len(nrow(x)), function(i) .canonical_tree(as.list(x[i, , drop = FALSE]))))
  }
  if (is.factor(x)) x <- as.character(x)
  if (!is.null(names(x))) {
    if (!.named(x)) .abort("Canonical object names must be unique and nonempty")
    x <- as.list(x)[order(enc2utf8(names(x)), method = "radix")]
  }
  if (is.list(x)) return(lapply(x, .canonical_tree))
  if (is.character(x)) x <- gsub("\r\n?", "\n", enc2utf8(x))
  x
}
.canonical_json <- function(x) {
  .forbidden_keys(x)
  enc2utf8(as.character(jsonlite::toJSON(.canonical_tree(x), auto_unbox = TRUE,
    digits = NA, null = "null", na = "null")))
}
.sha256 <- function(x) digest::digest(x, algo = "sha256", serialize = FALSE)
.hash <- function(x) .sha256(.canonical_json(x))
.runtime_fingerprint <- function(data) {
  inspect <- function(x) {
    if (is.environment(x) || is.function(x) || inherits(x, "connection") ||
        typeof(x) %in% c("externalptr", "weakref"))
      .abort("Data contains an unsupported runtime object", "evaluably_error_unserializable_data")
    if (is.list(x) || is.pairlist(x)) for (v in x) inspect(v)
    # Attributes can themselves hide external pointers or environments.
    a <- attributes(x)
    if (length(a)) for (v in a) inspect(v)
  }
  inspect(data)
  tryCatch(.sha256(serialize(data, NULL, version = 3, xdr = TRUE, ascii = FALSE)),
    error = function(e) .abort(conditionMessage(e), "evaluably_error_unserializable_data"))
}
.strip_srcref <- function(x) {
  attr(x, "srcref") <- NULL
  if (is.list(x)) x <- lapply(x, .strip_srcref)
  x
}
