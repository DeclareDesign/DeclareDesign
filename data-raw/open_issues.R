# Runs each open DeclareDesign issue that can be reproduced in code against one
# installed version of the package, and prints PASS, FAIL or ERROR per case.
# The source of the "Open issues in DeclareDesign" section of
# vignettes/declaredesign2.0.Rmd. Rbuildignored.
#
# Usage, once per version, each from a library holding that version with its
# matching fabricatr, randomizr and estimatr:
#   Rscript data-raw/open_issues.R 1.1.1 <library>
#   Rscript data-raw/open_issues.R 2.0.0 <library>
#
# The library is named explicitly and the version asserted, because the two
# versions share a package name: a run that silently loaded the other version
# from the default library would report the wrong package's behaviour.
args <- commandArgs(trailingOnly = TRUE)
stopifnot("usage: Rscript data-raw/open_issues.R <1.1.1|2.0.0> <library>" =
            length(args) == 2L && args[1] %in% c("1.1.1", "2.0.0"))
want <- args[1]
pkg <- if (want == "1.1.1") "DD" else "DD0"
LIB <- normalizePath(path.expand(args[2]), mustWork = TRUE)
.libPaths(c(LIB, .libPaths()))
suppressMessages({
  library(randomizr); library(estimatr); library(fabricatr); library(DeclareDesign)
  library(dplyr); library(broom)
  try(library(metafor), silent = TRUE)
})
for (p in c("DeclareDesign", "fabricatr", "estimatr", "randomizr")) {
  if (!identical(dirname(find.package(p)), LIB)) {
    stop(sprintf("%s loaded from %s, not %s", p, dirname(find.package(p)), LIB))
  }
}
stopifnot(identical(as.character(packageVersion("DeclareDesign")), want))
cat(sprintf("# %s: DeclareDesign %s from %s\n", pkg, packageVersion("DeclareDesign"), LIB))

report <- function(n, what, expr) {
  res <- tryCatch({
    val <- suppressWarnings(suppressMessages(force(expr)))
    if (isTRUE(val)) "PASS" else paste0("FAIL: ", val)
  }, error = function(e) paste0("ERROR: ", substr(gsub("\n", " ", conditionMessage(e)), 1, 70)))
  cat(sprintf("#%-4s %-34s %s\n", n, what, res))
}

# 463: metafor's rma passed directly as the method
report(463, "rma.uni as .method", {
  d <- declare_model(N = 5, estimate = rnorm(N), std.error = runif(N, .1, .2)) +
    if (pkg == "DD") declare_estimator(yi = estimate, sei = std.error, model = rma.uni)
    else declare_estimator(yi = estimate, sei = std.error, .method = rma.uni)
  e <- draw_estimates(d)
  if (nrow(e) > 0 && any(!is.na(e$estimate))) TRUE else "no non-NA estimate"
})

# 456: subset and weights with the plain lm handler
report(456, "lm with subset =", {
  d <- declare_model(N = 100, x = rnorm(N)) +
    if (pkg == "DD") declare_estimator(x ~ 1, model = lm, subset = x > 1)
    else declare_estimator(x ~ 1, .method = lm, subset = x > 1)
  nrow(draw_estimates(d)) > 0
})
report(456, "lm with weights =", {
  d <- declare_model(N = 100, x = rnorm(N)) +
    if (pkg == "DD") declare_estimator(x ~ 1, model = lm, weights = 1 / x^2)
    else declare_estimator(x ~ 1, .method = lm, weights = 1 / x^2)
  nrow(draw_estimates(d)) > 0
})

# 457: a handler's own default arguments
report(457, "handler default args", {
  f <- function(N = 10, a = 1) data.frame(X = rep(a, N))
  step <- if (pkg == "DD") declare_population(handler = f, N = 3)
          else declare_model(handler = f, N = 3)
  out <- step(NULL)
  if (nrow(out) == 3 && all(out$X == 1)) TRUE else "wrong shape"
})

# 479: spurious many-to-many warning
report(479, "no spurious m2m warning", {
  d <- declare_model(N = 100, U = rnorm(N)) +
    declare_inquiry(ATE = 1, ATE_1 = 2, ATE_2 = 3) +
    declare_estimator(U ~ 1, term = "(Intercept)", inquiry = "ATE", label = "est1") +
    declare_estimator(U ~ 1, term = "(Intercept)", inquiry = "ATE", label = "est2")
  w <- NULL
  withCallingHandlers(run_design(d),
    warning = function(x) { w <<- conditionMessage(x); invokeRestart("muffleWarning") })
  if (is.null(w)) TRUE else paste0("warned: ", substr(w, 1, 40))
})

# 293: a helper function whose own environment holds the parameter
report(293, "helper env retained after rm", {
  m <- 2
  f <- function(x) m * x
  u <- if (pkg == "DD") declare_population(N = 2, X1 = f(1))
       else declare_model(N = 2, X1 = f(1))
  design <- u + NULL
  rm(m)
  nrow(draw_data(design)) == 2
})

# 496: a factor column plus resampling
report(496, "factor column, no length warning", {
  d <- declare_model(N = 100, X = rep(c(0, 1), each = N / 2), U = rnorm(N, sd = .25),
                     f = factor(X), potential_outcomes(Y ~ 0.2 * Z + X + U)) +
    declare_assignment(Z = complete_ra(N)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z))
  w <- NULL
  withCallingHandlers(draw_data(d),
    warning = function(x) { w <<- conditionMessage(x); invokeRestart("muffleWarning") })
  if (is.null(w)) TRUE else paste0("warned: ", substr(w, 1, 45))
})

# 482: a test-only design, redesigned, then diagnosed
report(482, "redesign + test-only design", {
  N <- 50
  d <- declare_model(N = N) +
    declare_measurement(Y = rbinom(n = N, size = 1, prob = 0.55)) +
    declare_test(handler = function(data) tidy(prop.test(x = table(data$Y), p = 0.5)))
  test <- redesign(d, N = 100)
  sims <- simulate_design(test, sims = 5)
  nrow(diagnose_design(sims, bootstrap_sims = 0)$diagnosands_df) > 0
})

# 472: redesign should name its output by the parameter value, so that
# bind_rows(.id = "design") is labelled with something a reader can use.
# Declared through declare_parameters(), because 2.0 refuses to redesign a
# name the model step merely writes down.
report(472, "redesign names carry the value", {
  ds <- if (pkg == "DD") redesign(declare_model(N = 50, Y = rnorm(N)) +
                                   declare_inquiry(m = mean(Y)), N = c(10, 20))
        else redesign(declare_parameters(n = 50) + declare_model(N = n, Y = rnorm(N)) +
                        declare_inquiry(m = mean(Y)), n = c(10, 20))
  nm <- names(ds)
  if (is.null(nm) || !all(nzchar(nm))) "unnamed"
  else if (all(grepl("^design_[0-9]+$", nm))) paste0("positional only: ", paste(nm, collapse = ", "))
  else TRUE
})

# 464: lapply over a design list with a bare function
report(464, "lapply(designs, draw_estimates)", {
  nn <- 30
  d <- declare_model(N = nn, U = rnorm(N), Z = complete_ra(N), Y = U + Z) +
    declare_inquiry(ATE = 1) +
    (if (pkg == "DD") declare_estimator(Y ~ Z, model = lm, term = "Z", inquiry = "ATE")
     else declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE", label = "o"))
  ds <- redesign(d, nn = c(30, 40))
  length(lapply(ds, draw_estimates)) == 2
})

# 385: an estimator that errors on some draws
report(385, "estimator error is captured", {
  boom <- function(formula, data) stop("model failed to converge")
  d <- declare_model(N = 20, Y = rnorm(N), Z = rep(0:1, 10)) +
    (if (pkg == "DD") declare_estimator(Y ~ Z, model = boom)
     else declare_estimator(Y ~ Z, .method = boom, label = "b"))
  s <- simulate_design(d, sims = 3)
  if (is.data.frame(s)) TRUE else "no table returned"
})

# 497: supply a free parameter at draw time rather than via redesign()
report(497, "draw_data(design, theta = .5)", {
  mod <- if (pkg == "DD") declare_model(N = 20, e = rnorm(N), D = rbinom(N, 1, .5), Y = theta * D + e)
         else declare_model(N = 20, e = rnorm(N), D = rbinom(N, 1, .5), Y = theta * D + e)
  d <- mod + NULL
  nrow(draw_data(d, theta = 0.5)) == 20
})
report(497, "redesign() route works instead", {
  theta <- 0.1
  d <- declare_model(N = 20, e = rnorm(N), D = rbinom(N, 1, .5), Y = theta * D + e) + NULL
  nrow(draw_data(redesign(d, theta = 0.5))) == 20
})

# 509: a recursive helper defined in the calling environment
report(509, "recursive helper function", {
  tbl <- c(a = 0.5, b = 0.25)
  fp <- function(phase, prev = NULL) {
    p <- tbl[[phase]]
    if (is.character(prev)) p <- p / fp(prev)
    p
  }
  d <- declare_model(N = 20, x = rbinom(N, 1, fp("b", "a"))) + NULL
  nrow(draw_data(d)) == 20
})

# 417: add_level inside a design diagnosed under plan(multisession). The
# reporter's design is declared at top level, so the worker has to receive the
# step's environment as well as the function.
report(417, "add_level under multisession", {
  library(future)
  on.exit(plan(sequential), add = TRUE)
  plan(multisession, workers = 2)
  d <- if (pkg == "DD") declare_population(my_level = add_level(N = 5)) + NULL
       else declare_model(my_level = add_level(N = 5)) + NULL
  diagnose_design(d, diagnosands = declare_diagnosands(nsims = length(estimate)), sims = 4)
  TRUE
})

# 445: a global from an Imports reached inside a step, under multisession.
report(445, "imported global under multisession", {
  library(future)
  on.exit(plan(sequential), add = TRUE)
  plan(multisession, workers = 2)
  d <- declare_model(N = 10, Y = rnorm(N)) + declare_inquiry(m = dplyr::first(Y)) +
    (if (pkg == "DD") declare_estimator(Y ~ 1, model = lm, term = "(Intercept)", inquiry = "m", label = "e")
     else declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)", inquiry = "m", label = "e"))
  nrow(simulate_design(d, sims = 4)) > 0
})
