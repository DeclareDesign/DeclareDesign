#' Declare a custom data-handling step
#'
#' Wraps an arbitrary handler function as a step in the design. The handler
#' must accept `data` as its first argument and return a data frame.
#'
#' @param handler A function whose first argument is `data`.
#' @param ... Additional arguments forwarded (unevaluated, then evaluated in
#'   the caller's environment) to `handler`.
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
declare_step <- function(handler, ..., label = "custom_step", draws = 1L) {
  dots <- rlang::enquos(...)
  call <- sys.call()
  force(handler)
  fn <- function(data) {
    args <- lapply(dots, function(q) {
      rlang::eval_tidy(q, data = if (is.data.frame(data)) as.list(data) else NULL)
    })
    do.call(handler, c(list(data), args))
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
