deps <- function(constants = list(), functions = list(), packages = character()) list(packages = packages, constants = constants, functions = functions)
helper <- function(f, d = NULL) list(fn = f, depends_on = d)

test_that("declared constants are explicit, matched, and hash-bearing", {
  k <- 5
  f <- function(x) x + k
  a <- .callback_digest(f, deps(list(k = 5)))
  k <- 6
  expect_false(identical(a, .callback_digest(f, deps(list(k = 6)))))
  expect_error(.callback_digest(f, deps(list(k = 5))), class = "evaluably_error_callback_dependency_value_mismatch")
  expect_error(.callback_digest(f), "k", class = "evaluably_error_undeclared_callback_dependency")
  expect_warning(.callback_digest(function(x) x, deps(list(k = 5))), class = "evaluably_warning_unused_callback_dependency")
  expect_error(.callback_digest(f, deps(list(k = rep(1, 1001)))), "1000")
  expect_error(.callback_digest(f, deps(functions = c(k = strrep("a", 64)))), "named lists")
})
test_that("M1 same structure with different closure dependencies has different identity", {
  make <- function(k) function(x) x + k
  a <- make(1); b <- make(2)
  expect_identical(body(a), body(b))
  expect_false(identical(.callback_digest(a, deps(list(k = 1))), .callback_digest(b, deps(list(k = 2)))))
  root <- function(x) a(x) + b(x)
  d <- deps(functions = list(a = helper(a, deps(list(k = 1))), b = helper(b, deps(list(k = 2)))))
  m <- .resolve_callback(root, d)
  expect_false(identical(m$manifest$functions[["a"]], m$manifest$functions[["b"]]))
  expect_identical(m$digest, .resolve_callback(root, d)$digest)
  expect_identical(m$digest, .resolve_callback(root, deps(functions = rev(d$functions)))$digest)
})
test_that("M1 structurally identical functions on a path are distinct runtime nodes", {
  leaf <- function(x) x
  make <- function(h) function(x) h(x)
  a <- make(leaf); b <- make(a)
  expect_identical(body(a), body(b))
  d <- deps(functions = list(h = helper(a, deps(functions = list(h = helper(leaf))))))
  expect_type(.callback_digest(b, d), "character")
})
test_that("M1 genuine cycle is detected before needing recursive declaration expansion", {
  f <- function(x) g(x)
  g <- function(x) f(x)
  d <- deps(functions = list(g = helper(g, deps(functions = list(f = helper(f))))))
  expect_error(.callback_digest(f, d), "callback -> g -> f", class = "evaluably_error_callback_dependency_cycle")
})
test_that("M1 depth five is allowed and depth six fails", {
  make <- function(n) {
    if (!n) return(helper(function(x) x))
    h <- make(n - 1L)
    fn <- local({ child <- h$fn; function(x) child(x) })
    helper(fn, deps(functions = list(child = h)))
  }
  a <- make(5); b <- make(6)
  expect_type(.callback_digest(a$fn, a$depends_on), "character")
  expect_error(.callback_digest(b$fn, b$depends_on), class = "evaluably_error_callback_recursion_depth")
})
test_that("M1 legitimate off-path helpers are independently resolved", {
  h <- function(x) x * 2
  left <- function(x) h(x)
  right <- function(x) h(x) + 1
  root <- function(x) left(x) + right(x)
  hd <- deps(constants = list(unused = 1))
  d <- deps(functions = list(left = helper(left, deps(functions = list(h = helper(h, hd)))),
                            right = helper(right, deps(functions = list(h = helper(h, hd))))))
  warnings <- character()
  a <- withCallingHandlers(.callback_digest(root, d), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
  })
  # Two independent resolutions warn twice: no off-path memoization.
  expect_length(warnings, 2L)
  expect_match(warnings[[1]], "left -> h")
  expect_match(warnings[[2]], "right -> h")
  expect_identical(a, suppressWarnings(.callback_digest(root, d)))
})
test_that("namespace calls and shadowed names cannot bypass declaration", {
  expect_type(.callback_digest(function(x) stats::median(x)), "character")
  f <- function(x) digest::digest(x)
  expect_error(.callback_digest(f), class = "evaluably_error_undeclared_callback_dependency")
  expect_type(.callback_digest(f, deps(packages = paste0("digest::", utils::packageVersion("digest")))), "character")
  expect_error(.callback_digest(f, deps(packages = "digest::0.0.0")), class = "evaluably_error_callback_dependency_value_mismatch")
  local({
    mean <- function(x) x + 1
    expect_error(.callback_digest(function(x) mean(x)), class = "evaluably_error_undeclared_callback_dependency")
  })
})
test_that("callback manifest contains no runtime identity and ignores formatting", {
  a <- eval(parse(text = "function(x) { x + 1 }", keep.source = TRUE))
  b <- eval(parse(text = "function(x) {\n # comment\n x+1\n}", keep.source = TRUE))
  expect_identical(.callback_digest(a), .callback_digest(b))
  m <- .resolve_callback(a)$manifest
  expect_named(m, c("formals", "body", "packages", "constants", "functions", "base_symbols"))
  expect_type(.canonical_json(m), "character")
})
