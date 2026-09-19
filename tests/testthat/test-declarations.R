rep_args <- function() list(representation_id = "r", object_type = "UNWEIGHTED_MEMBERSHIP",
  feature_definition = c("B", "A"), aggregation_rule = "MEAN", transformation = "NONE",
  preprocessing = "NONE", missing_feature_policy = "DROP",
  feature_mapping = data.frame(declared_feature = c("B", "A"), resolved_feature = c("B", "A"),
                              mapping_rule_id = "identity", resolved = TRUE),
  derived_from_original = FALSE, equivalence_status = "NOT_APPLICABLE")
inf_args <- function() list(alpha = .05, test_family = "PERMUTATION_EXACT",
  alternative = "ONE_SIDED_GREATER", pvalue_convention = "INCLUSIVE_OBSERVED",
  exactness = "EXACT", reachability_basis = "EXACT_ENUMERATION",
  support_fn = function(spec, data) list(support_cardinality = 16L, min_attainable_p = .0625))

test_that("feature order participates while named weights normalize", {
  a <- rep_args(); r1 <- do.call(representation_spec, a)
  a$feature_definition <- rev(a$feature_definition)
  expect_error(do.call(representation_spec, a), "misaligned")
  a$feature_mapping <- a$feature_mapping[2:1, ]
  r2 <- do.call(representation_spec, a)
  expect_false(identical(r1$representation_hash, r2$representation_hash))
  expect_identical(r1$representation_rule_hash, r2$representation_rule_hash)
  a <- rep_args(); a$object_type <- "WEIGHTED_LINEAR"; a$weights <- c(A = 1, B = 2)
  r3 <- do.call(representation_spec, a)
  a$weights <- rev(a$weights)
  r4 <- do.call(representation_spec, a)
  expect_identical(r3$weights, c(B = 2, A = 1))
  expect_identical(r3$representation_hash, r4$representation_hash)
  expect_false(identical(r1$representation_rule_hash, r3$representation_rule_hash))
  a$object_type <- "UNWEIGHTED_MEMBERSHIP"
  expect_error(do.call(representation_spec, a), "forbids")
})
test_that("model identity accepts exactly one declared route", {
  a <- rep_args(); a$object_type <- "MODEL_PREDICTION"
  expect_error(do.call(representation_spec, a), "identity route")
  a$model_artifact_ref <- "model.rds"; a$model_artifact_hash <- strrep("a", 64)
  r <- do.call(representation_spec, a)
  expect_s3_class(r, "representation_spec")
  expect_null(r$weights)
  a$score_fn <- function(x) x
  expect_error(do.call(representation_spec, a), "exactly one")
  a$model_artifact_ref <- NULL; a$model_artifact_hash <- NULL
  expect_s3_class(do.call(representation_spec, a), "representation_spec")
  a$score_fn <- NULL; a$model_artifact_ref <- "m"; a$model_artifact_hash <- "NO_MACHINE_READABLE_ARTEFACT"
  expect_error(do.call(representation_spec, a), "SHA256")
})
test_that("equivalence and coverage are explicit author decisions", {
  a <- rep_args(); a$derived_from_original <- TRUE
  expect_error(do.call(representation_spec, a), "original_object_ref")
  a$original_object_ref <- "doi:example"; a$equivalence_status <- "EQUIVALENT"
  expect_error(do.call(representation_spec, a), "equivalence_basis")
  a$equivalence_status <- "UNKNOWN"
  expect_identical(do.call(representation_spec, a)$equivalence_status, "UNKNOWN")
  a$feature_mapping$resolved[1] <- FALSE; a$feature_mapping$resolved_feature[1] <- NA_character_
  expect_s3_class(do.call(representation_spec, a), "representation_spec")
  a$min_feature_coverage <- .75
  expect_error(do.call(representation_spec, a), class = "evaluably_error_feature_coverage")
})
test_that("declaration mutation is refused through normal list assignment", {
  r <- do.call(representation_spec, rep_args())
  expect_error(r$preprocessing <- "changed", "immutable")
  expect_error(r[["preprocessing"]] <- "changed", "immutable")
  expect_error(r["preprocessing"] <- "changed", "immutable")
  i <- do.call(inferential_rule, inf_args())
  expect_error(i$alpha <- .1, "immutable")
})
test_that("inferential conditional requirements and numeric domains fail closed", {
  a <- inf_args(); i <- do.call(inferential_rule, a)
  expect_s3_class(i, "inferential_rule")
  a$alpha <- 0
  expect_error(do.call(inferential_rule, a), "alpha")
  a <- inf_args(); a$support_fn <- NULL
  expect_error(do.call(inferential_rule, a), "exactly")
  a <- inf_args(); a$support_fn <- function(spec, data, extra) NULL
  expect_error(do.call(inferential_rule, a), "exactly")
  a <- inf_args(); a$reachability_basis <- "NOT_APPLICABLE"
  expect_error(do.call(inferential_rule, a), "forbidden")
  a$support_fn <- NULL
  expect_error(do.call(inferential_rule, a), "justification")
  a$justification <- "Declared analytic reason"
  expect_s3_class(do.call(inferential_rule, a), "inferential_rule")
})
test_that("external cardinality is descriptive and all external fields are hashed", {
  a <- inf_args(); a$support_fn <- NULL; a$reachability_basis <- "DECLARED_EXTERNAL"
  a$justification <- "External derivation"
  a$external_support <- list(min_attainable_p = .0625, support_cardinality = 999L,
    source = "external derivation", source_hash = "NO_MACHINE_READABLE_ARTEFACT", rule_version = "1")
  x <- do.call(inferential_rule, a)
  expect_identical(x$external_support$min_attainable_p, .0625)
  a$external_support$support_cardinality <- 1L
  y <- do.call(inferential_rule, a)
  expect_identical(y$external_support$min_attainable_p, .0625)
  expect_false(identical(x$inferential_rule_hash, y$inferential_rule_hash))
  a$external_support$source_hash <- "bad"
  expect_error(do.call(inferential_rule, a), "source_hash")
})
