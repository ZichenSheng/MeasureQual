test_that("prospective identity has exactly ten input components", {
  x <- as.list(setNames(rep(strrep("a", 64), 10), .member_fields))
  x$subject_id <- "S1"; x$context_id <- "X"
  h <- .audit_member_key(x)
  x$severity_grid <- c(.25, 1); x$terminal_state <- "NOT_EVALUABLE"
  x$record_id <- "synthetic-artifact"; x$spec_hash <- strrep("b", 64)
  x$subject_provenance <- "provenance correction"
  expect_identical(.audit_member_key(x), h)
  x$terminal_state <- "SUPPORTED_WITHIN_DOMAIN"
  expect_identical(.audit_member_key(x), h)
  x$subject_id <- "S2"
  expect_false(identical(.audit_member_key(x), h))
  x$subject_id <- "S1"; x$representation_hash <- strrep("b", 64)
  expect_false(identical(.audit_member_key(x), h))
  x$metric_hash <- NULL
  expect_error(.audit_member_key(x), "Missing")
})
test_that("claim comparability binds estimand rather than prose", {
  c1 <- list(claim_family = "recovery", estimand = "fraction at q", text = "claim one", scope = "scope A")
  h <- .claim_contract_hash(c1, "MEASUREMENT_CAPABILITY")
  c1$text <- "claim two"; c1$scope <- "scope B"
  expect_identical(.claim_contract_hash(c1, "MEASUREMENT_CAPABILITY"), h)
  c1$estimand <- "different quantity"
  expect_false(identical(.claim_contract_hash(c1, "MEASUREMENT_CAPABILITY"), h))
})
test_that("malformed prospective component hashes are rejected", {
 t<-toy_spec();x<-.member_components(t$spec);x$metric_hash<-"not-a-sha256"
 expect_error(.audit_member_key(x),class="evaluably_error_invalid_spec")
})
