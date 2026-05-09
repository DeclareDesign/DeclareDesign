#' Declare a custom data-handling step
#'
#' Wraps an arbitrary handler function as a step in the design. The handler
#' must accept `data` as its first argument and return a data frame.
#'
#' @param handler A function whose first argument is `data`.
#' @param ... Additional arguments forwarded (unevaluated, then evaluated in
#'   the caller's environment) to `handler`.
#' @param label Step label.
#' @return A `design_step`.
#' @export
#' @examples
#' step <- declare_step(handler = function(data, k) {
#'   data$X2 <- data$X * k
#'   data
#' }, k = 2)
#' df <- data.frame(X = 1:5)
#' step(df)
declare_step <- function(handler, ..., label = "custom_step") {
  dots <- rlang::enquos(...)
  call <- sys.call()
  force(handler)
  fn <- function(data) {
    args <- lapply(dots, function(q) {
      rlang::eval_tidy(q, data = if (is.data.frame(data)) as.list(data) else NULL)
    })
    do.call(handler, c(list(data), args))
  }
  build_step(
    fn          = fn,
    handler_expr = rlang::enexpr(handler),
    dots        = dots,
    step_type   = "custom",
    causal_type = "dgp",
    label       = label,
    call        = call,
    handler_fn  = handler
  )
}
