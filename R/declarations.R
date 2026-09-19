#' Declare a Measurement Representation
#'
#' Declare feature identity, feature order, processing and original-object
#' equivalence. The engine does not choose features, map identifiers, infer
#' coefficients or establish equivalence. This development snapshot does not
#' execute audits.
#' @param representation_id Nonempty identifier chosen by the author.
#' @param object_type Declared representation type.
#' @param feature_definition Unique nonempty feature names, in declared order.
#' @param aggregation_rule Declared aggregation vocabulary member.
#' @param transformation Declared transformation vocabulary member.
#' @param preprocessing Nonempty processing description, or `"NONE"`.
#' @param missing_feature_policy Declared missing-feature policy.
#' @param feature_mapping Data frame with declared_feature, resolved_feature,
#'   mapping_rule_id and resolved; rows must follow feature_definition exactly.
#' @param score_fn Optional R scoring closure. Declare every user dependency.
#' @param weights Optional named finite coefficients, required for WEIGHTED_LINEAR.
#' @param signs Optional named integer signs, required for SIGNED_MEMBERSHIP.
#' @param derived_from_original Logical author declaration.
#' @param original_object_ref Required reference when derived_from_original is TRUE.
#' @param equivalence_status Author-declared equivalence; UNKNOWN is not a negative finding.
#' @param equivalence_basis Required explanation when declaring EQUIVALENT.
#' @param min_feature_coverage Optional minimum coverage; the package supplies no threshold.
#' @param score_fn_depends_on NULL or list(packages, constants, functions). Helpers
#'   use nested list(fn, depends_on) declarations; never supply helper hashes.
#' @param model_artifact_ref Stable model reference for artifact identity route.
#' @param model_artifact_hash Lowercase SHA256 of model bytes; raw models are forbidden.
#' @return Immutable representation_spec with computed identity and rule hashes.
#' @section Errors:
#' Invalid declarations fail with evaluably_error_invalid_spec. Coverage failures
#' use evaluably_error_feature_coverage. Undeclared dependencies, mismatched
#' values, cycles and excessive callback depth fail with their named conditions.
#' @examples
#' r <- representation_spec("r1", "UNWEIGHTED_MEMBERSHIP", c("B", "A"),
#'   "MEAN", "NONE", "NONE", "DROP",
#'   data.frame(declared_feature = c("B", "A"), resolved_feature = c("B", "A"),
#'              mapping_rule_id = "identity", resolved = TRUE),
#'   derived_from_original = FALSE, equivalence_status = "NOT_APPLICABLE")
#' r$representation_hash
#' @export
representation_spec <- function(representation_id, object_type, feature_definition,
  aggregation_rule, transformation, preprocessing, missing_feature_policy, feature_mapping,
  score_fn = NULL, weights = NULL, signs = NULL, derived_from_original,
  original_object_ref = NULL, equivalence_status, equivalence_basis = NULL,
  min_feature_coverage = NULL, score_fn_depends_on = NULL,
  model_artifact_ref = NULL, model_artifact_hash = NULL) {
  if (missing(representation_id) || missing(object_type) || missing(feature_definition) ||
      missing(aggregation_rule) || missing(transformation) || missing(preprocessing) ||
      missing(missing_feature_policy) || missing(feature_mapping) || missing(derived_from_original) ||
      missing(equivalence_status)) .abort("All scientific representation declarations are required")
  .require_text(representation_id, "representation_id")
  .choice(object_type, c("UNWEIGHTED_MEMBERSHIP", "WEIGHTED_LINEAR", "SIGNED_MEMBERSHIP",
    "RANK_BASED", "MODEL_PREDICTION", "EXTERNAL_SCORE", "OTHER_DECLARED"), "object_type")
  .choice(aggregation_rule, c("MEAN", "SUM", "MEDIAN", "WEIGHTED_SUM", "RANK_MEAN", "SINGLE_FEATURE", "EXTERNAL_DECLARED"), "aggregation_rule")
  .choice(transformation, c("NONE", "ZSCORE_WITHIN_CONTEXT", "ZSCORE_GLOBAL", "LOG", "RANK", "QUANTILE", "EXTERNAL_DECLARED"), "transformation")
  .choice(missing_feature_policy, c("DROP", "ZERO_FILL", "MEAN_IMPUTE", "FAIL_IF_ANY_MISSING", "EXTERNAL_DECLARED"), "missing_feature_policy")
  .choice(equivalence_status, c("EQUIVALENT", "NOT_EQUIVALENT", "UNKNOWN", "NOT_APPLICABLE"), "equivalence_status")
  .require_text(preprocessing, "preprocessing")
  if (!is.character(feature_definition) || !length(feature_definition) || anyNA(feature_definition) ||
      any(!nzchar(feature_definition)) || anyDuplicated(feature_definition)) .abort("Invalid feature_definition")
  columns <- c("declared_feature", "resolved_feature", "mapping_rule_id", "resolved")
  if (!is.data.frame(feature_mapping) || !all(columns %in% names(feature_mapping)) ||
      nrow(feature_mapping) != length(feature_definition) ||
      !identical(feature_mapping$declared_feature, feature_definition) ||
      !is.character(feature_mapping$resolved_feature) || !is.character(feature_mapping$mapping_rule_id) ||
      anyNA(feature_mapping$mapping_rule_id) || any(!nzchar(feature_mapping$mapping_rule_id)) ||
      !is.logical(feature_mapping$resolved) || anyNA(feature_mapping$resolved)) .abort("Invalid or misaligned feature_mapping")
  if (any(feature_mapping$resolved & (is.na(feature_mapping$resolved_feature) | !nzchar(feature_mapping$resolved_feature))))
    .abort("Resolved features require identifiers")
  normalize <- function(x, field, sign = FALSE) {
    if (is.null(x)) return(NULL)
    if (!is.numeric(x) || any(!is.finite(x)) || !.named(x) || !setequal(names(x), feature_definition) || length(x) != length(feature_definition))
      .abort(paste("Invalid", field, "feature binding"))
    if (sign && (!is.integer(x) || any(!x %in% c(-1L, 1L)))) .abort("signs must be integers in {-1, 1}")
    x[feature_definition]
  }
  weights <- normalize(weights, "weights")
  signs <- normalize(signs, "signs", TRUE)
  if (object_type == "WEIGHTED_LINEAR" && is.null(weights)) .abort("WEIGHTED_LINEAR requires weights")
  if (object_type == "SIGNED_MEMBERSHIP" && (is.null(signs) || !is.null(weights))) .abort("SIGNED_MEMBERSHIP requires signs and forbids weights")
  if (object_type == "UNWEIGHTED_MEMBERSHIP" && (!is.null(weights) || !is.null(signs))) .abort("UNWEIGHTED_MEMBERSHIP forbids weights and signs")
  if (!is.logical(derived_from_original) || length(derived_from_original) != 1L || is.na(derived_from_original)) .abort("Invalid derived_from_original")
  if (derived_from_original) {
    .require_text(original_object_ref, "original_object_ref")
    if (equivalence_status == "NOT_APPLICABLE") .abort("Derived objects require an equivalence declaration")
  } else if (!is.null(original_object_ref) || equivalence_status != "NOT_APPLICABLE") .abort("An original reference requires derived_from_original")
  if (equivalence_status == "EQUIVALENT") .require_text(equivalence_basis, "equivalence_basis")
  if (!is.null(min_feature_coverage)) {
    .scalar_number(min_feature_coverage, 0, 1, "min_feature_coverage")
    if (mean(feature_mapping$resolved) < min_feature_coverage) .abort("Feature coverage below the declared minimum", "evaluably_error_feature_coverage")
  }
  if (mean(feature_mapping$resolved) < 1)
    .inform("Feature coverage is incomplete and satisfies the declared coverage policy", "evaluably_message_feature_coverage")
  route_a <- !is.null(score_fn)
  route_b <- !is.null(model_artifact_ref) || !is.null(model_artifact_hash)
  if (route_b && (!.text(model_artifact_ref) || !.sha_valid(model_artifact_hash))) .abort("Artifact route requires reference and byte SHA256")
  if (route_a && route_b) .abort("Choose exactly one scoring identity route")
  if (object_type %in% c("MODEL_PREDICTION", "EXTERNAL_SCORE", "OTHER_DECLARED") && !route_a && !route_b) .abort("This representation requires a scoring identity route")
  if (!route_a && !is.null(score_fn_depends_on)) .abort("Dependencies supplied without score_fn")
  resolved <- if (route_a) .resolve_callback(score_fn, score_fn_depends_on, "score_fn") else NULL
  x <- list(representation_id = representation_id, object_type = object_type,
    feature_definition = feature_definition, aggregation_rule = aggregation_rule,
    transformation = transformation, preprocessing = preprocessing, missing_feature_policy = missing_feature_policy,
    feature_mapping = feature_mapping, score_fn_hash = resolved$digest,
    score_fn_depends_on = resolved$manifest, weights = weights, signs = signs,
    derived_from_original = derived_from_original, original_object_ref = original_object_ref,
    equivalence_status = equivalence_status, equivalence_basis = equivalence_basis,
    min_feature_coverage = min_feature_coverage, model_artifact_ref = model_artifact_ref,
    model_artifact_hash = model_artifact_hash)
  x$representation_hash <- .hash(x)
  x$representation_rule_hash <- .hash(x[c("object_type", "aggregation_rule", "transformation", "preprocessing", "missing_feature_policy")])
  # The live callback is not serialized or included in identity.
  attr(x, "runtime_score_fn") <- score_fn
  attr(x, "score_declaration") <- score_fn_depends_on
  structure(x, class = "representation_spec")
}

#' Declare Inferential Support
#'
#' Declare the inferential procedure and its support source. Capacity counts do
#' not determine p-values. Alpha and statistical conventions are author decisions.
#' @param alpha Required numeric scalar strictly between zero and one.
#' @param test_family Declared test-family vocabulary member.
#' @param alternative Declared one- or two-sided alternative.
#' @param pvalue_convention Declared p-value convention.
#' @param exactness Declared exactness vocabulary member.
#' @param reachability_basis How attainable p-value support is established.
#' @param support_fn R closure with exactly (spec, data) formals; required for
#'   EXACT_ENUMERATION and ANALYTIC_BOUND, forbidden otherwise.
#' @param external_support Required only for DECLARED_EXTERNAL: list containing
#'   min_attainable_p, optional support_cardinality, source, source_hash, rule_version.
#'   Cardinality is descriptive only. This route requires human review.
#' @param justification Required text for DECLARED_EXTERNAL or NOT_APPLICABLE.
#' @param support_fn_depends_on Nested dependency declaration, or NULL for no user dependencies.
#' @return Immutable inferential_rule carrying the declaration and its hash.
#' @section Errors:
#' Invalid vocabulary, contradictory requirements and malformed external support
#' raise evaluably_error_invalid_spec. Callback dependency errors fail closed.
#' @examples
#' inferential_rule(0.05, "ASYMPTOTIC", "TWO_SIDED", "DECLARED_EXTERNAL",
#'   "ASYMPTOTIC", "NOT_APPLICABLE", justification = "Continuous-p procedure")
#' @export
inferential_rule <- function(alpha, test_family, alternative, pvalue_convention,
  exactness, reachability_basis, support_fn = NULL, external_support = NULL,
  justification = NULL, support_fn_depends_on = NULL) {
  if (missing(alpha) || missing(test_family) || missing(alternative) || missing(pvalue_convention) ||
      missing(exactness) || missing(reachability_basis)) .abort("All scientific inferential declarations are required")
  .scalar_number(alpha, 0, 1, "alpha", TRUE)
  .choice(test_family, c("PERMUTATION_EXACT", "PERMUTATION_MONTE_CARLO", "RANK_EXACT", "ASYMPTOTIC", "BOOTSTRAP", "DECLARED_EXTERNAL"), "test_family")
  .choice(alternative, c("ONE_SIDED_GREATER", "ONE_SIDED_LESS", "TWO_SIDED"), "alternative")
  .choice(pvalue_convention, c("INCLUSIVE_OBSERVED", "EXCLUSIVE_OBSERVED", "MID_P", "ADD_ONE_SMOOTHED", "DECLARED_EXTERNAL"), "pvalue_convention")
  .choice(exactness, c("EXACT", "APPROXIMATE", "ASYMPTOTIC", "MONTE_CARLO"), "exactness")
  .choice(reachability_basis, c("EXACT_ENUMERATION", "ANALYTIC_BOUND", "DECLARED_EXTERNAL", "NOT_APPLICABLE", "UNDETERMINED"), "reachability_basis")
  if (exactness == "EXACT" && test_family == "ASYMPTOTIC") .abort("EXACT cannot describe ASYMPTOTIC test_family")
  callback_required <- reachability_basis %in% c("EXACT_ENUMERATION", "ANALYTIC_BOUND")
  if (callback_required) {
    if (!is.function(support_fn) || !identical(names(formals(support_fn)), c("spec", "data"))) .abort("support_fn must have exactly (spec, data) formals")
  } else if (!is.null(support_fn) || !is.null(support_fn_depends_on)) .abort("support_fn is forbidden for this basis")
  if (reachability_basis %in% c("DECLARED_EXTERNAL", "NOT_APPLICABLE")) .require_text(justification, "justification")
  if (reachability_basis == "DECLARED_EXTERNAL") {
    req <- c("min_attainable_p", "source", "source_hash", "rule_version")
    if (!is.list(external_support) || !all(req %in% names(external_support)) ||
        !.named(external_support) || any(!names(external_support) %in% c(req, "support_cardinality"))) .abort("Malformed external_support")
    .scalar_number(external_support$min_attainable_p, 0, 1, "min_attainable_p")
    .require_text(external_support$source, "source")
    .require_text(external_support$rule_version, "rule_version")
    if (!.sha_valid(external_support$source_hash) && !identical(external_support$source_hash, "NO_MACHINE_READABLE_ARTEFACT")) .abort("Invalid external support source_hash")
    n <- external_support$support_cardinality
    if (is.null(n)) external_support$support_cardinality <- NA_integer_
    else if (length(n) != 1L || (!is.na(n) && (!is.numeric(n) || !is.finite(n) || n < 0 || n != floor(n)))) .abort("Invalid descriptive support_cardinality")
  } else if (!is.null(external_support)) .abort("external_support is forbidden for this basis")
  resolved <- if (callback_required) .resolve_callback(support_fn, support_fn_depends_on, "support_fn") else NULL
  x <- list(alpha = alpha, test_family = test_family, alternative = alternative,
    pvalue_convention = pvalue_convention, exactness = exactness, reachability_basis = reachability_basis,
    support_fn_hash = resolved$digest, support_fn_depends_on = resolved$manifest,
    external_support = external_support, justification = justification)
  x$inferential_rule_hash <- .hash(x)
  attr(x, "runtime_support_fn") <- support_fn
  attr(x, "support_declaration") <- support_fn_depends_on
  structure(x, class = "inferential_rule")
}

#' @export
`$<-.representation_spec` <- function(x, name, value) .abort("Declarations are immutable; construct a new declaration")
#' @export
`[[<-.representation_spec` <- function(x, ..., value) .abort("Declarations are immutable; construct a new declaration")
#' @export
`[<-.representation_spec` <- function(x, ..., value) .abort("Declarations are immutable; construct a new declaration")
#' @export
`$<-.inferential_rule` <- function(x, name, value) .abort("Declarations are immutable; construct a new declaration")
#' @export
`[[<-.inferential_rule` <- function(x, ..., value) .abort("Declarations are immutable; construct a new declaration")
#' @export
`[<-.inferential_rule` <- function(x, ..., value) .abort("Declarations are immutable; construct a new declaration")
