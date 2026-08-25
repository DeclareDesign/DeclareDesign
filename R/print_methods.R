#' Print a design
#'
#' Lists the steps as the calls that declared them, then the parameters and
#' objects the design's expressions refer to: the names [redesign()] can
#' change, the value each one currently holds, its kind, and which steps
#' would respond to a change. A design with no such names prints its steps
#' alone. [summary()] runs the design once as well and says what each step
#' did to the data.
#'
#' @param x A `design`.
#' @param ... Ignored.
#' @return The input invisibly.
#' @export
#' @method print design
#' @examples
#' d <- declare_model(N = 30, Y = rnorm(N)) + declare_inquiry(mu = mean(Y))
#' print(d)
print.design <- function(x, ...) {
  cat(design_heading(x), "\n\n", sep = "")
  for (i in seq_along(x)) cat(format_step_line(x[[i]], i), "\n", sep = "")
  print_design_objects(x)
  invisible(x)
}

design_heading <- function(x) {
  n <- length(x)
  paste0("Research design with ", n, " step", if (n != 1L) "s")
}

#' One line per step: its index, type and the call that declared it
#'
#' A step built inside the package with no call, which `construct_design()`
#' can produce, falls back to its verb.
#'
#' @keywords internal
#' @noRd
format_step_line <- function(step, i) {
  sprintf("Step %d (%s): %s", i, attr(step, "step_type") %||% "?",
          format_step_call(step))
}

format_step_call <- function(step) {
  call <- attr(step, "call")
  if (is.null(call)) return(paste0(step_verb(step), "(...)"))
  paste(trimws(deparse(call, width.cutoff = 500L)), collapse = " ")
}

print_design_objects <- function(x) {
  objects <- find_all_objects(x)
  if (nrow(objects) > 0) {
    cat("\nParameters and objects the design refers to:\n")
    print(objects)
  }
  notes <- design_notes(x)
  if (nrow(notes) > 0) {
    cat("\nNotes the design takes when it runs (not redesignable):\n")
    print(notes, row.names = FALSE)
  }
  invisible(NULL)
}

#' Print a design step
#'
#' @param x A `design_step`.
#' @param ... Ignored.
#' @return The input invisibly.
#' @export
#' @method print design_step
#' @examples
#' s <- declare_model(N = 10, Y = rnorm(N))
#' print(s)
print.design_step <- function(x, ...) {
  cat(sprintf("<design_step: %s [%s]>\n",
              attr(x, "label") %||% "?",
              attr(x, "step_type") %||% "?"))
  invisible(x)
}

#' Print a diagnosis
#'
#' Prints the table [format.diagnosis()] builds: one row of diagnosand
#' estimates per group, with bootstrap standard errors in parentheses on the
#' row beneath.
#'
#' @param x A `diagnosis`.
#' @param digits Number of decimal places.
#' @param ... Ignored.
#' @return The reshaped table invisibly.
#' @export
#' @method print diagnosis
#' @examples
#' design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
#'   declare_inquiry(ATE = 0) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
#' print(d)
print.diagnosis <- function(x, digits = 2, ...) {
  cat("Research design diagnosis\n")
  sims_df <- x$simulations_df
  if (!is.null(sims_df) && "sim_ID" %in% names(sims_df) && nrow(sims_df) > 0) {
    n <- suppressWarnings(max(sims_df$sim_ID, na.rm = TRUE))
    if (is.finite(n)) {
      msg <- sprintf("  %d simulations", n)
      if (!is.null(x$variance_decomposition)) {
        msg <- paste0(msg, sprintf(" [nested: %s]",
                                   x$variance_decomposition$draw_levels[1]))
      }
      cat(msg, "\n", sep = "")
    }
  }
  if (x$bootstrap_sims > 0 && is.data.frame(x$bootstrap_replicates)) {
    cat("  bootstrap standard errors in parentheses (",
        x$bootstrap_sims, " replicates)\n", sep = "")
  }
  match_note <- describe_inquiry_match(x$matched_on)
  if (!is.null(match_note)) cat("  ", match_note, "\n", sep = "")
  cat("\n")
  out <- format(x, digits = digits)
  print(out, row.names = FALSE)
  if (!is.null(x$variance_decomposition)) {
    cat("\nVariance decomposition:\n")
    vd <- dplyr::select(x$variance_decomposition,
                        -dplyr::any_of(c("n_sims", "draw_levels")))
    print(vd)
  }
  invisible(out)
}

#' Summarize a diagnosis
#'
#' The same report [print.diagnosis()] gives.
#'
#' @param object A `diagnosis`.
#' @param digits Number of decimal places.
#' @param ... Ignored.
#' @return The reshaped table invisibly.
#' @export
#' @method summary diagnosis
#' @examples
#' design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
#'   declare_inquiry(ATE = 0) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' summary(diagnose_design(design, sims = 5, bootstrap_sims = 0))
summary.diagnosis <- function(object, digits = 2, ...) {
  print(object, digits = digits)
}

#' Say how estimates were matched to inquiries, when it was not by label
#'
#' A labelled estimator matches on `inquiry`, which is the expected case and
#' goes unremarked. Anything else is worth a line in the diagnosis header: an
#' estimator with no `inquiry =` matched on `sim_ID` alone, or a handler
#' emitting its own grouping columns matched on those too. Returns `NULL` when
#' there is nothing to say.
#'
#' @keywords internal
#' @noRd
describe_inquiry_match <- function(matched_on) {
  if (is.null(matched_on)) return(NULL)
  expected <- c("design", "sim_ID", "inquiry")
  extra <- setdiff(matched_on, expected)
  if ("inquiry" %in% matched_on && length(extra) == 0) return(NULL)
  on <- setdiff(matched_on, c("design", "sim_ID"))
  on <- c(intersect("inquiry", on), setdiff(on, "inquiry"))
  if (length(on) == 0) {
    paste0("estimates matched to inquiries within each simulation ",
           "(no estimator named an inquiry)")
  } else {
    paste0("estimates matched to inquiries on ", paste(on, collapse = ", "),
           if (!"inquiry" %in% matched_on) " (no estimator named an inquiry)")
  }
}

#' Format a diagnosis for display
#'
#' Rounds the diagnosands, puts each bootstrap standard error in parentheses on
#' a second row beneath its estimate, and gives the columns display names
#' (`mean_estimand` becomes `Mean Estimand`). This is what [print.diagnosis()]
#' shows and what you want in front of `knitr::kable()`.
#'
#' The standard-error rows appear only when the diagnosis was bootstrapped. At
#' `bootstrap_sims = 0` there are no replicates, so the table is one row per
#' group and there are no parentheses.
#'
#' It is a `format()` method because that is what it does. R's `format()` is
#' the generic for "the character representation this object prints as", and
#' `print()` is documented as calling it. The three views of a diagnosis are
#' worth keeping straight: [get_diagnosands()] is wide and numeric with
#' `se(bias)` as a sibling column, [tidy()] is long and numeric with the
#' standard error and interval as fields of each row, and this is wide and
#' character with the standard error rendered beneath its estimate. Only the
#' first two can be computed on. This one is a rendering of the first, a pure
#' function of it and `digits`, and it carries strictly less information than
#' either: the interval is not in it and the precision is gone.
#'
#' `reshape_diagnosis()` is DeclareDesign's name for the same thing and calls
#' this method, so code written against DeclareDesign keeps working.
#'
#' It returns a `data.frame` rather than a tibble because every column is a
#' formatted string, and a tibble would print a `<chr>` row under a display
#' table.
#'
#' Redesign parameter columns keep their own names, since `prob_each` is the
#' argument the reader passed and not a phrase to be title-cased.
#'
#' DeclareDesign's `select` and `exclude` arguments are deliberately absent:
#' the result is a data frame, so `select()` already chooses columns from it,
#' and an argument that duplicates a verb is the wart this package exists to
#' remove.
#'
#' @param x A `diagnosis` object.
#' @param digits Number of decimal places.
#' @param ... Ignored.
#' @return A `data.frame` of formatted strings.
#' @export
#' @method format diagnosis
#' @examples
#' design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
#'   declare_inquiry(ATE = 0) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
#' format(d)
#'
#' # DeclareDesign's name for the same table
#' reshape_diagnosis(d)
#'
#' # choose columns with select(), on the data frame it returns
#' format(d) |> dplyr::select(Term, Bias, Power)
format.diagnosis <- function(x, digits = 2, ...) {
  diagnosis <- x
  if (!inherits(diagnosis, "diagnosis")) {
    stop("`diagnosis` must be a diagnosis object, from diagnose_design().")
  }
  diagnosands_df <- as.data.frame(diagnosis$diagnosands_df)
  diagnosand_names <- intersect(diagnosis$diagnosand_names,
                                names(diagnosands_df))
  se_names <- intersect(paste0("se(", diagnosand_names, ")"),
                        names(diagnosands_df))
  group_cols <- setdiff(names(diagnosands_df), c(diagnosand_names, se_names))
  param_names <- diagnosis_parameter_names(diagnosis)

  estimate_rows <- diagnosands_df[c(group_cols, diagnosand_names)]
  estimate_rows[diagnosand_names] <-
    lapply(estimate_rows[diagnosand_names], format_num, digits = digits)

  if (length(se_names) == length(diagnosand_names) && length(se_names) > 0) {
    se_rows <- estimate_rows
    se_rows[group_cols] <- ""
    se_rows[diagnosand_names] <-
      lapply(diagnosands_df[se_names], add_parens, digits = digits)
    n <- nrow(estimate_rows)
    return_df <- rbind(estimate_rows, se_rows)[as.vector(rbind(1:n, 1:n + n)), ]
  } else {
    return_df <- estimate_rows
  }

  keep <- setdiff(names(return_df), param_names)
  names(return_df)[match(keep, names(return_df))] <- make_nice_names(keep)

  rownames(return_df) <- NULL
  return_df
}

#' @rdname format.diagnosis
#' @family diagnosands
#' @param diagnosis A `diagnosis` object.
#' @export
reshape_diagnosis <- function(diagnosis, digits = 2) {
  format(diagnosis, digits = digits)
}

#' The redesign parameters a diagnosis varies
#'
#' These are the columns [format.diagnosis()] leaves alone when it title-cases
#' the rest, since a parameter name is the argument the reader passed. A
#' diagnosis this package produced carries them on the simulations table; one
#' produced by DeclareDesign and read back in (from a course's `saved/`
#' directory, say) carries them as `parameters_df`, whose first column is the
#' design label.
#'
#' @keywords internal
#' @noRd
diagnosis_parameter_names <- function(diagnosis) {
  from_sims <- attr(diagnosis$simulations_df, "parameter_names")
  if (!is.null(from_sims)) return(from_sims)
  if (is.data.frame(diagnosis$parameters_df)) {
    return(setdiff(names(diagnosis$parameters_df), "design"))
  }
  character(0)
}

#' Give a diagnosands table column a display name
#'
#' `mean_estimand` becomes `Mean Estimand`, `sd_estimate` becomes
#' `SD Estimate`, `rmse` becomes `RMSE`, `se(bias)` becomes `SE(Bias)`.
#'
#' @keywords internal
#' @noRd
make_nice_names <- function(x) {
  gsub("\\b(se[(]|sd |rmse|[[:alpha:]])", "\\U\\1", gsub("_", " ", x),
       perl = TRUE)
}

#' @keywords internal
#' @noRd
format_num <- function(x, digits = 2) {
  sprintf(paste0("%.", digits, "f"), as.numeric(x))
}

#' @keywords internal
#' @noRd
add_parens <- function(x, digits = 2) {
  out <- sprintf("(%s)", format_num(x, digits))
  out[is.na(x)] <- "NA"
  out
}

#' Summarize a design
#'
#' Runs the design once and reports what each step did: the rows a data step
#' produced or kept and the columns it added, dropped or changed; the value
#' each inquiry took; the estimate each estimator returned. The run itself,
#' estimates joined to estimands, comes after the steps, then the parameters
#' and objects the design refers to. Everything the printout shows is on the
#' returned object, so a document or an app can use the pieces.
#'
#' The run is one random draw, so the numbers differ from call to call. A
#' design whose estimator is slow can be summarized without running it:
#' `run = FALSE` gives the steps and the parameters alone, which is what
#' [print()] shows.
#'
#' @param object A `design`.
#' @param run Whether to run the design once. Defaults to `TRUE`.
#' @param ... Ignored.
#' @return A `summary.design`: a list with `steps` (a tibble with one row per
#'   step: `step`, `label`, `type`, `call`, and `one_run`, the account of that
#'   step on this run), `parameters` and `notes` (as [design_parameters()]
#'   and [design_notes()] return them), `ran`, and when the design was run,
#'   `data` (the final data), `inquiries` and `estimates` (as [run_design()]
#'   returns them).
#' @export
#' @method summary design
#' @examples
#' n_units <- 30
#' design <- declare_model(N = n_units, U = rnorm(N), Y = U) +
#'   declare_inquiry(mu = mean(Y)) +
#'   declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)",
#'                     inquiry = "mu", label = "ols")
#' summary(design)
#' summary(design, run = FALSE)
#' summary(design)$steps
summary.design <- function(object, run = TRUE, ...) {
  steps <- unclass(object)
  rows <- tibble::tibble(
    step = seq_along(steps),
    label = names(steps),
    type = vapply(steps, function(s) attr(s, "step_type") %||% "?", character(1)),
    call = vapply(steps, format_step_call, character(1))
  )
  out <- list(
    steps = rows,
    parameters = find_all_objects(object),
    notes = design_notes(object),
    ran = isTRUE(run)
  )
  if (isTRUE(run)) {
    r <- run_design_internal(object, trace = TRUE)
    out$steps$one_run <- account_for_run(steps, r$trace)
    out$data <- r$data
    out$inquiries <- r$inquiries
    out$estimates <- merge_estimates_inquiries(r$estimates, r$inquiries)
  }
  structure(out, class = "summary.design")
}

#' What each step did on one run, in a sentence
#'
#' @keywords internal
#' @noRd
account_for_run <- function(steps, trace) {
  before <- NULL
  vapply(seq_along(steps), function(i) {
    tr <- trace[[i]]
    if (is.null(tr)) return(NA_character_)
    if (!is.null(tr$data)) {
      txt <- describe_data_change(before, tr$data)
      before <<- tr$data
      return(txt)
    }
    if (!is.null(tr$inquiries)) return(describe_inquiry_rows(tr$inquiries))
    if (!is.null(tr$estimates)) return(describe_estimate_rows(tr$estimates))
    if (!is.null(tr$notes)) {
      return(paste(names(tr$notes), vapply(tr$notes, describe_value, character(1)),
                   sep = " = ", collapse = ", "))
    }
    NA_character_
  }, character(1))
}

describe_data_change <- function(before, after) {
  if (!is.data.frame(after)) return("returned no data frame")
  n_after <- nrow(after)
  if (is.null(before)) {
    parts <- paste0("N = ", n_after, " row", if (n_after != 1L) "s")
    added <- names(after)
  } else {
    n_before <- nrow(before)
    parts <- if (n_after != n_before) {
      paste0("keeps ", n_after, " of ", n_before, " rows")
    } else character(0)
    added <- setdiff(names(after), names(before))
    dropped <- setdiff(names(before), names(after))
    common <- intersect(names(before), names(after))
    changed <- if (n_after == n_before) {
      common[!vapply(common, function(nm) identical(before[[nm]], after[[nm]]),
                     logical(1))]
    } else character(0)
    if (length(dropped)) parts <- c(parts, paste("drops", paste(dropped, collapse = ", ")))
    if (length(changed)) parts <- c(parts, paste("changes", paste(changed, collapse = ", ")))
  }
  if (length(added)) parts <- c(parts, paste("adds", paste(added, collapse = ", ")))
  if (!length(parts)) parts <- "leaves the data as it was"
  paste(parts, collapse = "; ")
}

describe_inquiry_rows <- function(inq) {
  if (!is.data.frame(inq) || !nrow(inq)) return("no estimand")
  if (!all(c("inquiry", "estimand") %in% names(inq))) {
    return(paste0(nrow(inq), " row", if (nrow(inq) != 1L) "s"))
  }
  paste(inq$inquiry, format(inq$estimand, digits = 3), sep = " = ",
        collapse = ", ")
}

describe_estimate_rows <- function(est) {
  if (!is.data.frame(est) || !nrow(est)) return("no estimate")
  if ("error" %in% names(est) && any(est$error %in% TRUE)) {
    msg <- est$error_message[est$error %in% TRUE][[1]]
    return(paste0("failed: ", msg))
  }
  if (!"estimate" %in% names(est)) {
    return(paste0(nrow(est), " row", if (nrow(est) != 1L) "s"))
  }
  term <- if ("term" %in% names(est)) est$term else rep("estimate", nrow(est))
  se <- if ("std.error" %in% names(est)) {
    paste0(" (std.error ", format(est$std.error, digits = 3), ")")
  } else ""
  paste0(term, " = ", format(est$estimate, digits = 3), se, collapse = ", ")
}

#' @export
#' @method print summary.design
print.summary.design <- function(x, ...) {
  n <- nrow(x$steps)
  cat("Research design with ", n, " step", if (n != 1L) "s", "\n\n", sep = "")
  for (i in seq_len(n)) {
    cat(sprintf("Step %d (%s): %s\n", i, x$steps$type[i], x$steps$call[i]))
    if (x$ran && !is.na(x$steps$one_run[i])) {
      cat("  ", x$steps$one_run[i], "\n", sep = "")
    }
  }
  if (x$ran) {
    if (nrow(x$estimates) > 0) {
      cat("\nOne run of the design:\n")
      print(x$estimates)
    } else if (nrow(x$inquiries) > 0) {
      cat("\nOne run of the design:\n")
      print(x$inquiries)
    }
  }
  if (nrow(x$parameters) > 0) {
    cat("\nParameters and objects the design refers to:\n")
    print(x$parameters)
  }
  if (nrow(x$notes) > 0) {
    cat("\nNotes the design takes when it runs (not redesignable):\n")
    print(x$notes, row.names = FALSE)
  }
  invisible(x)
}

#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy a diagnosis
#'
#' Returns the diagnosands in long form, one row per group per diagnosand, with
#' the bootstrap standard error and percentile confidence interval alongside.
#' This is the shape to plot from: `ggplot(aes(estimate, diagnosand)) +
#' geom_errorbarh(aes(xmin = conf.low, xmax = conf.high))`.
#'
#' `std.error`, `conf.low`, and `conf.high` come from the bootstrap replicates,
#' so they appear only when the diagnosis was run with `bootstrap_sims > 0`.
#'
#' @param x A `diagnosis`.
#' @param conf.int Whether to include the confidence interval. Defaults to
#'   `TRUE`.
#' @param conf.level Confidence level for the interval. Defaults to 0.95.
#' @param ... Ignored.
#' @return A tibble.
#' @importFrom generics tidy
#' @export tidy.diagnosis
#' @export
#' @method tidy diagnosis
#' @examples
#' design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
#'   declare_inquiry(ATE = 0) +
#'   declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
#'                     label = "ols")
#' d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
#' tidy(d)
tidy.diagnosis <- function(x, conf.int = TRUE, conf.level = 0.95, ...) {
  df <- x$diagnosands_df
  diag_names <- intersect(x$diagnosand_names, names(df))
  if (length(diag_names) == 0) return(tibble::as_tibble(df))
  se_names <- paste0("se(", diag_names, ")")
  group_cols <- setdiff(names(df), c(diag_names, se_names, "n_sims"))

  out <- df |>
    dplyr::select(dplyr::all_of(c(group_cols, diag_names))) |>
    tidyr::pivot_longer(dplyr::all_of(diag_names), names_to = "diagnosand",
                        values_to = "estimate")

  if (all(se_names %in% names(df))) {
    ses <- df |>
      dplyr::select(dplyr::all_of(c(group_cols, se_names))) |>
      tidyr::pivot_longer(dplyr::all_of(se_names), names_to = "diagnosand",
                          values_to = "std.error") |>
      dplyr::mutate(diagnosand = sub("^se\\((.*)\\)$", "\\1", .data$diagnosand))
    out <- dplyr::left_join(out, ses, by = c(group_cols, "diagnosand"))
  }

  if (isTRUE(conf.int) && is.data.frame(x$bootstrap_replicates)) {
    alpha <- 1 - conf.level
    intervals <- x$bootstrap_replicates |>
      dplyr::select(dplyr::all_of(c(group_cols, diag_names))) |>
      tidyr::pivot_longer(dplyr::all_of(diag_names), names_to = "diagnosand",
                          values_to = "replicate") |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "diagnosand")))) |>
      dplyr::summarize(
        conf.low  = quantile_na(.data$replicate, alpha / 2),
        conf.high = quantile_na(.data$replicate, 1 - alpha / 2),
        .groups   = "drop"
      )
    out <- dplyr::left_join(out, intervals, by = c(group_cols, "diagnosand"))
  }

  out
}

#' Percentile of a bootstrap replicate vector, NA if any replicate is NA
#'
#' A diagnosand that fails on some replicate (no estimand, say) has no
#' interval, and reporting the quantile of the replicates that happened to
#' work would hide that.
#'
#' @keywords internal
#' @noRd
quantile_na <- function(x, probs) {
  if (anyNA(x)) return(NA_real_)
  unname(stats::quantile(x, probs))
}
