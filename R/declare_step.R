#' Does this handler need its arguments as quosures?
#'
#' `fabricate()` is the one handler that wants neither values nor bare
#' expressions. It captures its dots with `enquos()` and evaluates them in a
#' mask of its own that binds names the caller never wrote, `N` above all. A
#' bare expression arrives carrying the environment of the call we build rather
#' than the user's, so `fabricate(y = x + rnorm(N, 0, 0))` fails on
#' `object 'N' not found`; spliced quosures keep their own environment and it
#' works. Measured both ways, 2026-08-04, which is why this special case
#' survived the move of the other handlers onto the as-written convention.
#'
#' Both spellings count: a script carried over from DeclareDesign passes
#' `fabricatr::fabricate`, and dispatching on the fabricatrZero function alone
#' sent it down the wrong branch and failed inside fabricate.
#'
#' @keywords internal
#' @noRd
handler_is_fabricate <- function(handler) {
  if (identical(handler, fabricatrZero::fabricate)) return(TRUE)
  if (requireNamespace("fabricatr", quietly = TRUE)) {
    return(identical(handler, fabricatr::fabricate))
  }
  FALSE
}

#' Declare a custom data-handling step
#'
#' Wraps an arbitrary handler function as a step in the design. The handler
#' must accept `data` as its first argument and return a data frame.
#'
#' @family design declarations
#' @param handler A function whose first argument is `data`.
#' @param ... Additional arguments passed to `handler` **as written**, so the
#'   handler evaluates them itself. A bare column name arrives as a name, which
#'   is what lets `tidyr::pivot_wider(id_cols = pair)` and the dplyr verbs
#'   select and mask as they normally do. This is the same rule
#'   [declare_estimator()] follows. A handler that does no evaluation of its own
#'   and wants a value computed from the data takes a closure instead:
#'   `declare_step(handler = function(data) f(data, cutoff = mean(data$a)))`.
#' @param label Step label.
#' @param draws Number of nested draws for this step. When `> 1`, the step is
#'   re-executed `draws` times for each upstream draw during nested simulation.
#' @return A `design_step`.
#' @export
#' @examples
#' step <- declare_step(handler = function(data, k) {
#'   data$X2 <- data$X * k
#'   data
#' }, k = 2)
#' df <- data.frame(X = 1:5)
#' step(df)
#'
#' # A tidyselect handler receives the column names, not their contents.
#' wide <- declare_step(
#'   id_cols = pair, names_from = role, values_from = c(ID, a),
#'   handler = tidyr::pivot_wider
#' )
#' long <- data.frame(pair = rep(1:3, each = 2), role = rep(c("A", "B"), 3),
#'                    ID = sprintf("%03d", 1:6), a = 1:6)
#' wide(long)
declare_step <- function(handler, ..., label = "custom_step", draws = 1L) {
  dots <- capture_dots_env(rlang::enquos(...))
  call <- sys.call()
  force(handler)
  # Taken at declaration time, so it is where the user wrote the call and not
  # whatever is on the stack during a simulation.
  decl_env <- dots_env(dots, default = rlang::caller_env())
  args <- dots_as_written(dots)
  fn <- function(data) {
    if (handler_is_fabricate(handler)) {
      rlang::inject(handler(data = data, !!!dots))
    } else {
      # `.dd_data` holds this step's data frame for the duration of one call.
      # Arguments go in as written, so anything the handler resolves against
      # the data it resolves itself.
      call_env <- rlang::env(decl_env, .dd_data = data)
      eval(rlang::call2(handler, quote(.dd_data), !!!args), envir = call_env)
    }
  }
  step <- build_step(
    fn          = fn,
    handler_expr = rlang::enexpr(handler),
    dots        = dots,
    step_type   = "custom",
    causal_type = "dgp",
    label       = label,
    call        = call,
    handler_fn  = handler
  )
  attr(step, "draws") <- as.integer(draws)
  step
}
