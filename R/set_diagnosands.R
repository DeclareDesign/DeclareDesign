#' Attach diagnosands to a design
#'
#' Stores `diagnosands` as an attribute on `design`; [diagnose_design()] will
#' use these in place of the defaults when present.
#'
#' @param design A `design`.
#' @param diagnosands A diagnosands `design_step`. Defaults to
#'   [default_diagnosands()].
#' @return The design with `diagnosands` attribute set.
#' @export
#' @examples
#' design <- declare_model(N = 30, Y = rnorm(N))
#' design <- set_diagnosands(design,
#'   declare_diagnosands(mean_Y = mean(estimate, na.rm = TRUE)))
#' attr(design, "diagnosands") |> attr("step_type")
set_diagnosands <- function(design, diagnosands = default_diagnosands()) {
  attr(design, "diagnosands") <- diagnosands
  design
}

#' Subset diagnosands by name
#'
#' @param diagnosands A diagnosands `design_step`.
#' @param ... Names of diagnosands to keep.
#' @return A diagnosands `design_step` containing only the selected entries.
#' @export
#' @examples
#' diags <- default_diagnosands()
#' kept <- select_diagnosands(diags, "bias", "rmse")
#' names(attr(kept, "dots"))
select_diagnosands <- function(diagnosands, ...) {
  keep <- c(...)
  if (is.list(keep) && length(keep) == 1 && is.character(keep[[1]])) {
    keep <- keep[[1]]
  }
  dots <- attr(diagnosands, "dots")
  kept <- dots[intersect(names(dots), keep)]
  out <- diagnosands
  attr(out, "dots") <- kept
  fn <- function(data) {
    out_list <- purrr::imap(kept, function(q, nm) {
      val <- rlang::eval_tidy(q, data = as.list(data))
      tibble::tibble(diagnosand = nm, value = val)
    })
    dplyr::bind_rows(out_list)
  }
  for (a in setdiff(names(attributes(diagnosands)), names(attributes(fn)))) {
    attr(fn, a) <- attr(diagnosands, a)
  }
  attr(fn, "dots") <- kept
  class(fn) <- class(diagnosands)
  fn
}

#' Attach a citation to a design (stub)
#'
#' Records bibliographic information on the design. This is currently a
#' lightweight stub.
#'
#' @param design A `design`.
#' @param ... Citation fields (e.g., `title`, `author`, `year`).
#' @return The design with a `citation` attribute set.
#' @export
#' @examples
#' design <- declare_model(N = 10, Y = rnorm(N))
#' set_citation(design, title = "Example", author = "Coppock", year = 2026)
set_citation <- function(design, ...) {
  attr(design, "citation") <- list(...)
  design
}

#' Retrieve a design's citation (stub)
#'
#' @param design A `design`.
#' @return The citation list, or `NULL` if none has been set.
#' @export
#' @examples
#' design <- set_citation(declare_model(N = 10, Y = rnorm(N)),
#'                        title = "Example")
#' cite_design(design)
cite_design <- function(design) {
  attr(design, "citation")
}
