# The source of the three-row speed table in vignettes/declaredesign2.0.Rmd and
# of the clustered-agreement sentence below it. Rbuildignored.
#
# Run: Rscript data-raw/benchmark_vignette.R
#
# Written 2026-09-12, modelled on fabricatr's data-raw/benchmark_vignette.R,
# which exists because a stale table there hid a 1.6x regression for three
# weeks. DeclareDesign's table had no such guard: its figures were measured on
# the 2026-08-24 build and nothing re-derived them afterwards.
#
# Four things about this script are load-bearing.
#
# First, every cell runs in its own R process. estimatr's harness found cells
# whose time depended on how much the heap had grown before they ran, a
# five-fold difference for one of them, and a benchmark whose value depends on
# which benchmark ran before it is not a measurement.
#
# Second, the comparison holds estimatr fixed. The vignette compares
# "DeclareDesign 1.1.1 with fabricatr 1.0.2 against DeclareDesign 2.0.0 with
# fabricatr 2.0.0", and the estimator is the same `lm_robust()` call on both
# sides. That is deliberate: it isolates the plumbing, which is what the
# rewrite changed. Swapping estimatr underneath as well would measure two
# rewrites at once and attribute the total to this one.
#
# Third, repetition is not the same as isolation. estimatr's 2026-09-12 session
# recorded a timing that reproduced in its own process and was still wrong,
# because fifteen replications were too few; at forty the ratio moved and
# changed sign. So each process takes the median of REPS replications, and the
# reported figure is the smallest of RUNS such medians. A laptop is never idle,
# and keeping the quietest reading is what makes the absolute seconds mean
# anything. The ratios survive noise that the seconds do not.
#
# Fourth, the closing checks read the vignette back. A measured number living
# in a document that nothing re-derives is the copy that goes stale unnoticed,
# which is the defect this script exists to prevent.

LIB111 <- file.path(tools::R_user_dir("DeclareDesign", "cache"), "lib111")
LIB200 <- file.path(tools::R_user_dir("DeclareDesign", "cache"), "lib200")
RUNS <- 3L
REPS <- 5L
VIGNETTE <- "vignettes/declaredesign2.0.Rmd"

# The 1.x comparison library ----
# DeclareDesign 1.1.1 and fabricatr 1.0.2 are both current on CRAN, so this is
# an ordinary install rather than an archive fetch. estimatr is deliberately
# absent: the process picks it up from the main library, the same build on both
# sides of the comparison.
if (!dir.exists(file.path(LIB111, "DeclareDesign"))) {
  dir.create(LIB111, showWarnings = FALSE, recursive = TRUE)
  install.packages(c("DeclareDesign", "fabricatr"), lib = LIB111,
                   repos = "https://cloud.r-project.org", quiet = TRUE)
}
stopifnot(dir.exists(file.path(LIB111, "DeclareDesign")),
          dir.exists(file.path(LIB111, "fabricatr")))

# The 2.0.0 side is installed from this source tree on every run, so the table
# describes HEAD rather than whatever happens to be in the user library. The
# first run of this script did NOT do that: it left the 2.0.0 column on the
# default library path, which on a developer's machine holds CRAN 1.1.1, so it
# timed 1.1.1 against itself and reported ratios of 1.08x and 1.04x. Hence the
# version assertion inside every cell below. A benchmark that cannot say which
# build it loaded is not a measurement (2026-09-12).
unlink(LIB200, recursive = TRUE)
dir.create(LIB200, showWarnings = FALSE, recursive = TRUE)
message("installing the source tree into the 2.0.0 comparison library")
st <- system2("R", c("CMD", "INSTALL", paste0("--library=", shQuote(LIB200)),
                     "--no-docs", "--no-byte-compile", "."),
              stdout = FALSE, stderr = FALSE)
stopifnot("could not install the source tree for the 2.0.0 column" = st == 0L,
          dir.exists(file.path(LIB200, "DeclareDesign")))

# The design, written so both versions accept it ----
# `inquiry` takes a character label in 1.1.1 and requires one in 2.0.0, and the
# potential outcomes are written out by hand rather than through
# `potential_outcomes()` so that neither version is doing extra work the other
# is not. Both build the same five steps over the same columns.
DESIGN_SRC <- '
  tau <- 0.3
  design <-
    declare_model(N = 600, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + tau) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = complete_ra(N)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, .method = lm_robust, inquiry = "ATE", label = "ols")
'
# Both versions accept `.method`, checked rather than assumed, so the two sides
# run the identical source. Substituting 1.x's older `model =` spelling would
# have routed the call through a different argument on one side only.
DESIGN_SRC_111 <- DESIGN_SRC

ROWS <- list(
  list(key = "simulate500",
       label = "`simulate_design(sims = 500)`",
       expr  = "simulate_design(design, sims = 500)"),
  list(key = "diagnose500",
       label = "`diagnose_design(sims = 500, bootstrap_sims = 200)`",
       expr  = "diagnose_design(design, sims = 500, bootstrap_sims = 200)")
)

# One cell, in its own process ----
time_cell <- function(lib, want_version, design_src, expr, reps, plan_workers = NA_integer_) {
  script <- tempfile(fileext = ".R")
  writeLines(c(
    sprintf('.libPaths(c(%s, .libPaths()))', deparse(lib)),
    'suppressMessages({library(DeclareDesign); library(fabricatr)',
    '                  library(randomizr); library(estimatr)})',
    sprintf('stopifnot("this cell loaded the wrong DeclareDesign" = as.character(packageVersion("DeclareDesign")) == %s)',
            deparse(want_version)),
    if (!is.na(plan_workers))
      sprintf('suppressMessages(future::plan(future::multisession, workers = %d))', plan_workers)
    else 'suppressMessages(future::plan(future::sequential))',
    design_src,
    # Worker startup is a one-time session cost, not part of what parallelism
    # costs per run, and the claim being checked is what a user gets after
    # `future::plan()` has already been called. Timing it would have charged
    # four process spawns to the parallel column and understated the gain; at
    # two replications it was half the measurement (2026-09-12).
    if (!is.na(plan_workers)) 'invisible(simulate_design(design, sims = 8))' else "",
    sprintf('ts <- numeric(%d)', reps),
    sprintf('for (i in seq_len(%d)) {', reps),
    '  set.seed(343)',
    sprintf('  ts[i] <- system.time(%s)[["elapsed"]]', expr),
    '}',
    'future::plan(future::sequential)',
    # Tagged, and parsed by the tag. Taking the last line of output instead cost
    # the parallel row its first run: `multisession` workers each emit R's
    # "built under a different version" warning, which lands after the number
    # (2026-09-12).
    'cat("BENCH_TIMING", median(ts), "\\n")'
  ), script)
  out <- suppressWarnings(system2("Rscript", script, stdout = TRUE, stderr = TRUE))
  on.exit(unlink(script), add = TRUE)
  hit <- grep("^BENCH_TIMING ", out, value = TRUE)
  if (!length(hit))
    stop("cell produced no timing: ", expr, "\n", paste(utils::tail(out, 15), collapse = "\n"))
  as.numeric(sub("^BENCH_TIMING ", "", utils::tail(hit, 1)))
}

quietest <- function(...) min(vapply(seq_len(RUNS), function(i) time_cell(...), numeric(1)))

# The sequential rows ----
cat("Timing", length(ROWS), "sequential rows,", RUNS, "runs of", REPS,
    "replications each, per version.\n\n")
res <- lapply(ROWS, function(r) {
  t111 <- quietest(lib = LIB111, want_version = "1.1.1", design_src = DESIGN_SRC_111, expr = r$expr, reps = REPS)
  t200 <- quietest(lib = LIB200, want_version = "2.0.0", design_src = DESIGN_SRC,     expr = r$expr, reps = REPS)
  cat(sprintf("  %-52s 1.1.1 %6.2f s   2.0.0 %6.2f s   %.2fx\n",
              r$label, t111, t200, t111 / t200))
  list(key = r$key, label = r$label, t111 = t111, t200 = t200, ratio = t111 / t200)
})

# The parallel row ----
# Both halves are 2.0.0; the row reports what `future::plan()` buys, not what
# the rewrite buys, so 1.1.1 does not appear in it.
cat("\nTiming the parallel row (2.0.0 only).\n")
par_seq <- quietest(lib = LIB200, want_version = "2.0.0", design_src = DESIGN_SRC,
                    expr = "simulate_design(design, sims = 2000)", reps = 5L)
par_par <- quietest(lib = LIB200, want_version = "2.0.0", design_src = DESIGN_SRC,
                    expr = "simulate_design(design, sims = 2000)", reps = 5L,
                    plan_workers = 4L)
cat(sprintf("  %-52s %6.2f s parallel against %6.2f s sequential   %.1fx\n",
            "`simulate_design(sims = 2000)`, four workers",
            par_par, par_seq, par_seq / par_par))

# The clustered-agreement sentence ----
# Not a timing. The vignette claims the two packages agree on what they report
# for a clustered design, which is a stronger claim than either speed row and
# is the one a reader would actually act on.
cat("\nClustered design, 2,000 simulations, both versions.\n")
CLUSTER_SRC <- '
  design <-
    declare_model(
      village = add_level(N = 30, v_u = rnorm(N, sd = sqrt(0.2))),
      citizen = add_level(N = 20, U = rnorm(N, sd = sqrt(0.8)),
                          Y_Z_0 = v_u + U, Y_Z_1 = v_u + U + 0.3)) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = cluster_ra(clusters = village)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
'
diag_cell <- function(lib, want_version, src, est_arg) {
  script <- tempfile(fileext = ".R")
  writeLines(c(
    sprintf('.libPaths(c(%s, .libPaths()))', deparse(lib)),
    'suppressMessages({library(DeclareDesign); library(fabricatr)',
    '                  library(randomizr); library(estimatr)})',
    sprintf('stopifnot("this cell loaded the wrong DeclareDesign" = as.character(packageVersion("DeclareDesign")) == %s)',
            deparse(want_version)),
    paste0(src, est_arg),
    'set.seed(343)',
    # 2,000 rather than the 500 the vignette used to quote. The two columns are
    # independent runs, because 2.0's seeds are deliberately not 1.x's, so every
    # difference between them is Monte Carlo error. At 500 sims the standard
    # error on coverage is about 0.010 and on power about 0.021, which is the
    # same size as the gaps being shown; at 2,000 it is halved and the claim
    # that the two packages agree is one the numbers can actually carry.
    'd <- diagnose_design(design, sims = 2000, bootstrap_sims = 0)',
    'g <- as.data.frame(d$diagnosands_df)',
    'pick <- function(n) { v <- g[[n]]; if (is.null(v)) NA_real_ else v[1] }',
    'cat(sprintf("BENCH_DIAG %.3f %.3f %.3f %.3f\\n", pick("bias"), pick("sd_estimate"),',
    '            pick("power"), pick("coverage")))'
  ), script)
  on.exit(unlink(script), add = TRUE)
  out <- suppressWarnings(system2("Rscript", script, stdout = TRUE, stderr = TRUE))
  hit <- grep("^BENCH_DIAG ", out, value = TRUE)
  if (!length(hit))
    stop("diagnosis cell produced nothing\n", paste(utils::tail(out, 15), collapse = "\n"))
  as.numeric(strsplit(trimws(sub("^BENCH_DIAG ", "", utils::tail(hit, 1))), " +")[[1]])
}
c111 <- diag_cell(LIB111, "1.1.1", CLUSTER_SRC,
                  'declare_estimator(Y ~ Z, .method = lm_robust, clusters = village, inquiry = "ATE")')
c200 <- diag_cell(LIB200, "2.0.0", CLUSTER_SRC,
                  'declare_estimator(Y ~ Z, .method = lm_robust, clusters = village, inquiry = "ATE")')
nm <- c("bias", "sd_estimate", "power", "coverage")
for (i in seq_along(nm))
  cat(sprintf("  %-12s 1.1.1 %7.3f   2.0.0 %7.3f\n", nm[i], c111[i], c200[i]))

# What the vignette should say ----
cat("\n---- table for", VIGNETTE, "----\n")
cat("| Operation | 1.1.1 | 2.0.0 | Ratio |\n|---|---:|---:|---:|\n")
for (r in res)
  cat(sprintf("| %s | %.2f s | %.2f s | %.2fx |\n", r$label, r$t111, r$t200, r$ratio))
cat(sprintf("| `simulate_design(sims = 2000)`, four workers | | %.2f s against %.2f s sequential | %.1fx |\n",
            par_par, par_seq, par_seq / par_par))
cat(sprintf("\nclustered agreement sentence: bias %.3f against %.3f, SD of the estimate %.3f against %.3f, power %.3f against %.3f, coverage %.3f against %.3f\n",
            c111[1], c200[1], c111[2], c200[2], c111[3], c200[3], c111[4], c200[4]))

# Closing checks ----
# Read the vignette back and say which quoted ratios this run no longer
# supports. The comparison is a tolerance, not a string match. Four runs of this
# script on one idle machine produced 1.13x to 1.20x on the first row and 3.3x
# to 3.5x on the parallel row, so an exact-match check fails on almost every run
# and a guard that cries wolf every time is one nobody reads. fabricatr's
# harness already learned this; TOL is the same idea. The point of the check is
# to catch a figure that has moved beyond noise, not to pin the last digit.
TOL <- 0.12

if (file.exists(VIGNETTE)) {
  lines <- readLines(VIGNETTE, warn = FALSE)
  said <- as.numeric(sub(".*\\|\\s*([0-9.]+)x\\s*\\|\\s*$", "\\1",
                        grep("^\\|.*[0-9]x \\|$", lines, value = TRUE)))
  got <- c(res[[1]]$ratio, res[[2]]$ratio, par_seq / par_par)
  cat("\n---- checks against", VIGNETTE, "----\n")
  if (length(said) != length(got)) {
    cat("  the vignette's table is not the", length(got), "rows this script measures\n")
  } else {
    for (i in seq_along(got))
      cat(sprintf("  %-52s vignette %.2fx   this run %.2fx   %s\n",
                  if (i <= length(res)) res[[i]]$label else "parallel row",
                  said[i], got[i],
                  if (abs(said[i] - got[i]) <= TOL) "within tolerance" else "*** MOVED ***"))
    if (all(abs(said - got) <= TOL))
      cat("\nthe speed claims in the vignette hold\n")
    else
      cat("\nat least one ratio has moved beyond", TOL, "- update the table printed above\n")
  }
}
