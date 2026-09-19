.empty_dependencies <- function() list(packages = character(), constants = list(), functions = list())
.auto_packages <- c("base", "stats", "utils", "methods", "graphics", "grDevices", "tools")
.binding <- function(symbol, env) {
  while (!identical(env, emptyenv())) {
    if (exists(symbol, env, inherits = FALSE)) return(list(value = get(symbol, env, inherits = FALSE), env = env))
    env <- parent.env(env)
  }
  NULL
}
.binding_package <- function(env) {
  n <- environmentName(env)
  if (identical(env, baseenv())) return("base")
  if (isNamespace(env)) return(getNamespaceName(env))
  if (startsWith(n, "package:")) return(substring(n, 9L))
  ""
}
.qualified_calls <- function(expr) {
  out <- list()
  walk <- function(x) {
    if (is.call(x)) {
      if (is.symbol(x[[1L]]) && as.character(x[[1L]]) %in% c("::", ":::")) {
        if (length(x) != 3L || !is.symbol(x[[2L]]) || !is.symbol(x[[3L]]))
          .abort("Namespace calls require literal package and symbol names")
        out[[length(out) + 1L]] <<- c(as.character(x[[2L]]), as.character(x[[3L]]))
      }
      for (v in as.list(x)) walk(v)
    } else if (is.pairlist(x) || is.expression(x)) {
      # Missing formals are skipped by deparse/findGlobals; traverse defaults only.
      for (i in seq_along(x)) if (!identical(x[[i]], quote(expr = ))) walk(x[[i]])
    }
  }
  walk(expr)
  out
}
.validate_dependencies <- function(d) {
  if (is.null(d)) d <- .empty_dependencies()
  if (!is.list(d) || !setequal(names(d), c("packages", "constants", "functions")) || length(d) != 3L)
    .abort("depends_on must contain exactly packages, constants, functions")
  if (!is.character(d$packages) || anyNA(d$packages) || anyDuplicated(d$packages) ||
      any(!grepl("^[A-Za-z][A-Za-z0-9.]*::[^:]+$", d$packages))) .abort("Invalid declared packages")
  if (anyDuplicated(sub("::.*$", "", d$packages))) .abort("A package may have only one declared version")
  if (!is.list(d$constants) || !.named(d$constants) || !is.list(d$functions) || !.named(d$functions))
    .abort("constants and functions must be named lists; raw helper hashes are forbidden")
  if (length(intersect(names(d$constants), names(d$functions)))) .abort("Dependency names must have one role")
  valid_constant <- function(x) {
    if (is.object(x) || is.environment(x) || is.function(x)) return(FALSE)
    if (is.list(x)) return(all(vapply(x, valid_constant, logical(1))))
    is.atomic(x) && typeof(x) %in% c("logical", "integer", "double", "character", "complex", "raw", "NULL")
  }
  for (n in names(d$constants)) {
    v <- d$constants[[n]]
    if (!valid_constant(v) || length(unlist(v, recursive = TRUE)) > 1000L || length(v) > 1000L)
      .abort(paste("Constant", n, "must contain at most 1000 atomic values"))
    d$constants[n] <- list(.strip_srcref(v))
  }
  for (n in names(d$functions)) {
    h <- d$functions[[n]]
    if (!is.list(h) || length(h) != 2L || !setequal(names(h), c("fn", "depends_on")) || !is.function(h$fn))
      .abort(paste("Helper", n, "requires list(fn, depends_on)"))
  }
  d
}
.resolve_callback <- function(f, depends_on = NULL, role = "callback", ancestors = list(), path = character()) {
  if (!is.function(f) || is.primitive(f)) .abort(paste(role, "must be an R closure"))
  current <- c(path, role)
  label <- paste(current, collapse = " -> ")
  # Actual function-node reference identity is ephemeral and never enters M.
  if (any(vapply(ancestors, function(a) rlang::is_reference(a, f), logical(1))))
    .abort(paste("Callback dependency cycle:", label), "evaluably_error_callback_dependency_cycle")
  if (length(ancestors) > 5L)
    .abort(paste("Callback depth exceeds 5:", label), "evaluably_error_callback_recursion_depth")
  d <- .validate_dependencies(depends_on)
  declared_pkgs <- sub("::.*$", "", d$packages)
  versions <- sub("^.*::", "", d$packages)
  for (i in seq_along(declared_pkgs)) {
    p <- declared_pkgs[[i]]
    if (!requireNamespace(p, quietly = TRUE) || as.character(utils::packageVersion(p)) != versions[[i]])
      .abort(paste("Package version mismatch:", p, "in", label), "evaluably_error_callback_dependency_value_mismatch")
  }
  g <- codetools::findGlobals(f, merge = FALSE)
  symbols <- sort(unique(c(g$functions, g$variables)), method = "radix")
  base_symbols <- character()
  children <- character()
  for (s in symbols) {
    b <- .binding(s, environment(f))
    if (is.null(b)) .abort(paste("Undeclared or unbound dependency", s, "in", label), "evaluably_error_undeclared_callback_dependency")
    p <- .binding_package(b$env)
    if (p %in% .auto_packages) {
      base_symbols <- c(base_symbols, s)
    } else if (p %in% declared_pkgs) {
      next
    } else if (s %in% names(d$constants)) {
      if (!identical(.strip_srcref(b$value), d$constants[[s]]))
        .abort(paste("Declared/resolved constant mismatch:", s, "in", label), "evaluably_error_callback_dependency_value_mismatch")
    } else if (s %in% names(d$functions)) {
      if (!is.function(b$value) || !identical(b$value, d$functions[[s]]$fn, ignore.srcref = TRUE))
        .abort(paste("Declared/resolved helper mismatch:", s, "in", label), "evaluably_error_callback_dependency_value_mismatch")
      child <- .resolve_callback(b$value, d$functions[[s]]$depends_on, s, c(ancestors, list(f)), current)
      children[s] <- child$digest
    } else .abort(paste("Undeclared dependency", s, "class", paste(class(b$value), collapse = "/"), "in", label),
                  "evaluably_error_undeclared_callback_dependency")
  }
  qualified <- c(.qualified_calls(body(f)), .qualified_calls(formals(f)))
  for (q in qualified) {
    p <- q[[1L]]
    if (!p %in% c(.auto_packages, declared_pkgs))
      .abort(paste("Undeclared namespace dependency", paste(q, collapse = "::"), "in", label),
             "evaluably_error_undeclared_callback_dependency")
    if (!requireNamespace(p, quietly = TRUE) || !exists(q[[2L]], asNamespace(p), inherits = FALSE))
      .abort(paste("Unresolved namespace dependency", paste(q, collapse = "::")), "evaluably_error_callback_dependency_value_mismatch")
    if (p %in% .auto_packages) base_symbols <- c(base_symbols, paste(q, collapse = "::"))
  }
  unused <- setdiff(c(names(d$constants), names(d$functions)), symbols)
  for (s in unused) rlang::warn(paste("Declared but unused dependency", s, "in", label),
                              class = c("evaluably_warning_unused_callback_dependency", "evaluably_condition"), code = "evaluably_warning_unused_callback_dependency")
  # Resolve every occurrence, including retained unused declarations. No memoization.
  for (s in setdiff(names(d$functions), names(children))) {
    h <- d$functions[[s]]
    children[s] <- .resolve_callback(h$fn, h$depends_on, s, c(ancestors, list(f)), current)$digest
  }
  manifest <- list(
    formals = deparse(formals(f), control = c("keepInteger", "keepNA")),
    body = deparse(body(f), control = c("keepInteger", "keepNA")),
    packages = sort(d$packages, method = "radix"),
    constants = d$constants[order(as.character(names(d$constants)), method = "radix")],
    functions = children[order(as.character(names(children)), method = "radix")],
    base_symbols = sort(unique(base_symbols), method = "radix")
  )
  list(digest = .hash(manifest), manifest = manifest)
}
.callback_digest <- function(f, depends_on = NULL, role = "callback") .resolve_callback(f, depends_on, role)$digest
