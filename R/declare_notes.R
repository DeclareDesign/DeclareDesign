#' Declare quantities computed while a design runs
#'
#' @description
#' Records a named quantity at a point in the pipeline and makes it available
#' to every step that follows. A note is computed when the design runs, at the
#' position it is declared, from whatever is in scope there: the parameters,
#' the notes already taken, and the data as they stand at that moment.
#'
#' ```
#' declare_model(N = 100, Y = rnorm(N)) +
#'   declare_notes(tallest = max(Y)) +
#'   declare_sampling(S = complete_rs(N, n = 10)) +
#'   declare_inquiry(shortfall = mean(tallest - Y))
#' ```
#'
#' `tallest` is the largest value in the population, and the inquiry reads it
#' after sampling has thrown most of that population away. Notes are what
#' carry a quantity across a step that destroys the data it was computed from.
#'
#' @details
#' A note is fixed from the moment it is taken until the end of that run. It
#' is not recomputed by the steps that read it, so `tallest` above stays the
#' population maximum rather than becoming the sample maximum. It is redrawn
#' on the next run, because the data are. Declaring the same name again later
#' overwrites it from that point on and leaves the earlier steps alone.
#'
#' A note that reads no data is the ordinary way to write a quantity derived
#' from a parameter:
#'
#' ```
#' declare_parameters(m_arms = 3) +
#'   declare_notes(ks = seq_len(m_arms)[-1],
#'                 term_names = paste0("factor(Z)", ks))
#' ```
#'
#' `m_arms` is the design's one knob and `redesign()` changes it. `ks` and
#' `term_names` follow from it, and are not knobs: they cannot be set
#' independently without contradicting `m_arms`, so [redesign()] refuses them
#' and [design_parameters()] does not list them. That is the difference
#' between a note and a parameter. A [declare_parameters()] step names what a
#' caller may change; a `declare_notes()` step names what the design works out
#' for itself.
#'
#' Within one call, each note may read the ones declared before it. A note may
#' not share a name with a declared parameter.
#'
#' The step generates no data and adds no rows to a design's output.
#'
#' @family design declarations
#' @seealso [declare_parameters()] for the quantities a caller may change,
#'   [design_notes()] for the notes a design takes.
#' @param ... Named quantities. Each is evaluated when the design runs, in
#'   order, and may read the parameters, the notes taken before it, and the
#'   current data.
#' @param label Step label.
#' @return A `design_step`.
#' @export
#' @examples
#' # A quantity that outlives the data it is computed from.
#' design <-
#'   declare_model(N = 100, Y = rnorm(N)) +
#'   declare_notes(population_mean = mean(Y)) +
#'   declare_sampling(S = complete_rs(N, n = 10)) +
#'   declare_inquiry(gap = mean(Y) - population_mean)
#' draw_estimands(design)
#'
#' # A quantity derived from a parameter, so that `m_arms` is the only knob.
#' design <-
#'   declare_parameters(m_arms = 3) +
#'   declare_notes(inquiry_names = paste0("ate_", seq_len(m_arms)[-1])) +
#'   declare_model(N = 60, u = rnorm(N)) +
#'   declare_inquiry(handler = function(data) {
#'     data.frame(inquiry = inquiry_names, estimand = 0)
#'   })
#' design_parameters(design)
#' design_notes(design)
#' draw_estimands(redesign(design, m_arms = 4))
declare_notes <- function(..., label = "notes") {
  dots <- rlang::enquos(...)
  nms <- names(dots) %||% rep("", length(dots))
  if (length(dots) && !all(nzchar(nms))) {
    stop("Every note must be named, as in `declare_notes(tallest = max(Y))`.",
         call. = FALSE)
  }
  if (anyDuplicated(nms)) {
    dup <- unique(nms[duplicated(nms)])
    stop("Note declared more than once in one call: ",
         paste(dup, collapse = ", "), ".", call. = FALSE)
  }
  dots <- capture_dots_env(dots)
  build_step(
    fn           = function(data) data,
    handler_expr = NULL,
    dots         = dots,
    step_type    = "notes",
    causal_type  = "notes",
    label        = label,
    call         = sys.call()
  )
}

#' Is this step a note declaration?
#'
#' @keywords internal
#' @noRd
is_notes_step <- function(step) {
  identical(attr(step, "step_type"), "notes")
}

#' Every note name declared anywhere in a design
#'
#' @keywords internal
#' @noRd
declared_note_names <- function(design) {
  steps <- if (inherits(design, "design")) unclass(design) else design
  nms <- lapply(steps, function(s) {
    if (!is_notes_step(s)) return(character(0))
    names(attr(s, "dots")) %||% character(0)
  })
  unique(unlist(nms, use.names = FALSE)) %||% character(0)
}

#' Evaluate one note declaration against the run so far
#'
#' Each note is evaluated in order with the notes already taken bound in its
#' environment, and with the current data as the mask. Putting the earlier
#' notes in the environment rather than the mask is what makes a column win a
#' name clash, which is the rule declared parameters already follow: a name
#' the data supplies belongs to the data.
#'
#' `data` is `NULL` for a note declared before any data exists, which is the
#' case for a note derived from a parameter alone.
#'
#' @keywords internal
#' @noRd
note_values <- function(step, data = NULL, prior = list()) {
  dots <- attr(step, "dots")
  out <- list()
  for (nm in names(dots)) {
    quo <- dots[[nm]]
    taken <- c(prior, out)
    expr <- rlang::quo_get_expr(quo)
    reads <- intersect(names(taken), expr_symbols(expr))
    if (length(reads)) {
      # A child of the quosure's own environment rather than a copy of it, so
      # the notes shadow everything the declaration could already see and
      # nothing the declaration owns is disturbed. A literal carries the empty
      # environment, which cannot be a parent.
      env <- rlang::quo_get_env(quo)
      if (!rlang::is_environment(env) || identical(env, rlang::empty_env())) {
        env <- rlang::base_env()
      }
      quo <- rlang::new_quosure(
        expr,
        env = rlang::new_environment(data = taken[reads], parent = env)
      )
    }
    value <- tryCatch(
      list(rlang::eval_tidy(quo, data = data)),
      error = function(e) {
        stop("Note `", nm, "` could not be computed: ", conditionMessage(e),
             call. = FALSE)
      }
    )
    out[nm] <- value
  }
  out
}

#' Fold a fresh set of notes into the ones already taken
#'
#' A repeated name replaces the earlier value rather than adding a second one,
#' which is what makes a later `declare_notes()` an overwrite.
#'
#' @keywords internal
#' @noRd
record_notes <- function(notes, new) {
  for (nm in names(new)) notes[nm] <- new[nm]
  notes
}

#' Push the notes taken so far onto the steps that follow
#'
#' The same rebinding a declared parameter gets, run during the simulation
#' rather than when the design is built, and applied to a copy of the step
#' list so nothing leaks from one run into the next. Note environments carry
#' their own marker, so a second note declaration replaces the first one
#' rather than stacking on it, and neither disturbs the parameter environment
#' underneath.
#'
#' @keywords internal
#' @noRd
apply_notes_from <- function(steps, i, notes) {
  if (!length(notes)) return(steps)
  for (j in seq_along(steps)) {
    if (j <= i || is_notes_step(steps[[j]]) || is_parameters_step(steps[[j]])) {
      next
    }
    steps[[j]] <- bind_params_into_step(
      steps[[j]], notes,
      marker = "dd_note_env", applied_attr = "notes_applied"
    )
  }
  steps
}

#' Refuse a design that declares one name as both a parameter and a note
#'
#' The two answer opposite questions about the same name: a parameter is a
#' knob a caller may turn, a note is a quantity the design works out. A design
#' that declares both has no answer for `redesign()`, so it is caught when the
#' step is added rather than on the draw that disagrees.
#'
#' @keywords internal
#' @noRd
check_notes_against_params <- function(steps) {
  notes <- declared_note_names(steps)
  if (!length(notes)) return(invisible(NULL))
  params <- declared_param_names(steps)
  clash <- intersect(notes, params)
  if (length(clash)) {
    stop("Declared as both a parameter and a note: ",
         paste(clash, collapse = ", "), ".\n",
         "A parameter is a knob `redesign()` turns; a note is computed from ",
         "one. Pick one for each name.", call. = FALSE)
  }
  invisible(NULL)
}

#' Report the notes a design takes
#'
#' @description
#' Lists the names a [declare_notes()] step declares and the expression each
#' is computed from. Unlike [design_parameters()], no value is reported: a
#' note has one only while the design is running, and a note computed from
#' data has a different one on every draw.
#'
#' The two functions divide a design's names between them. What
#' `design_parameters()` lists can be changed with [redesign()]; what
#' `design_notes()` lists cannot, and follows from what can.
#'
#' @family modifying a design
#' @seealso [design_parameters()].
#' @param design A `design` or a `design_step`.
#' @return A data frame with one row per note: `name`, `expr` and `step`.
#' @export
#' @examples
#' design <- declare_parameters(m_arms = 3) +
#'   declare_notes(ks = seq_len(m_arms)[-1]) +
#'   declare_model(N = 60)
#' design_notes(design)
design_notes <- function(design) {
  if (inherits(design, "design_step")) {
    design <- construct_design(wrap_step(design))
  }
  if (!inherits(design, "design")) {
    stop("`design` must be a `design` or `design_step` object.")
  }
  steps <- unclass(design)
  rows <- list()
  for (i in seq_along(steps)) {
    if (!is_notes_step(steps[[i]])) next
    dots <- attr(steps[[i]], "dots")
    for (nm in names(dots)) {
      rows[[length(rows) + 1L]] <- data.frame(
        name = nm,
        expr = rlang::as_label(rlang::quo_get_expr(dots[[nm]])),
        step = i,
        stringsAsFactors = FALSE
      )
    }
  }
  out <- if (length(rows) == 0) {
    data.frame(name = character(0), expr = character(0), step = integer(0),
               stringsAsFactors = FALSE)
  } else {
    do.call(rbind, rows)
  }
  row.names(out) <- NULL
  out
}
