#' Compare two designs
#'
#' @param design1 A design object, typically created using the + operator
#' @param design2 A design object, typically created using the + operator
#' @param format Format (in console or HTML) options from \code{diffobj::diffChr}
#' @param mode Mode options from \code{diffobj::diffChr}
#' @param ... Options sent to \code{diffobj::diffChr}
#'
#' @examples
#' 
#' design1 <- declare_population(N = 100, u = rnorm(N)) +
#'   declare_potential_outcomes(Y ~ Z + u) +
#'   declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(n = 75) +
#'   declare_assignment(m = 50) +
#'   declare_reveal(Y, Z) +
#'   declare_estimator(Y ~ Z, estimand = "ATE")
#' 
#' design2 <- declare_population(N = 200, u = rnorm(N)) +
#'   declare_potential_outcomes(Y ~ 0.5*Z + u) +
#'   declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(n = 100) +
#'   declare_assignment(m = 25) +
#'   declare_reveal(Y, Z) +
#'   declare_estimator(Y ~ Z, model = lm_robust, estimand = "ATE")
#'  
#'  compare_designs(design1, design2)
#'  compare_design_code(design1, design2)
#'  compare_design_summaries(design1, design2)
#'  compare_design_data(design1, design2)
#'  compare_design_estimates(design1, design2)
#'  compare_design_estimands(design1, design2)
#' 
#' @name compare_functions

#' @rdname compare_functions
#' @export
compare_designs <- function(design1, design2, format = "ansi8", mode = "sidebyside", pager = "off", context = -1L) {
  
  code_comparison <- compare_design_code(design1, design2, format = format, mode = mode, pager = pager, context = context)
  
  data_comparison <- compare_design_data(design1, design2, format = format, mode = mode, pager = pager, context = context)
  
  estimands_comparison <- compare_design_estimands(design1, design2, format = format, mode = mode, pager = pager, context = context)
  
  estimates_comparison <- compare_design_estimates(design1, design2, format = format, mode = mode, pager = pager, context = context)
  
  structure(
    list(code_comparison = code_comparison, data_comparison = data_comparison, 
         estimands_comparison = estimands_comparison, estimates_comparison = estimates_comparison),
    class = "design_comparison"
  )
  
}

#' @rdname compare_functions
#' @export
print.design_comparison <- function(x, ...) {
  cat("Research design comparison\n\n")
  
  print_console_header("Compare design code")
  
  print(x$code_comparison)
  
  print_console_header("Compare draw_data(design)")
  
  print(x$data_comparison)
  
  print_console_header("Compare draw_estimands(design)")
  
  print(x$estimands_comparison)
  
  print_console_header("Compare draw_estimates(design)")
  
  print(x$estimates_comparison)
}


#' @rdname compare_functions
#' @importFrom diffobj diffChr
#' @export
compare_design_code <- function(design1, design2, format = "ansi8", mode = "sidebyside", pager = "off", context = -1L) {
  
  design1 <- get_design_code(design1)
  design2 <- get_design_code(design2)
   
  structure(
    diffChr(
      design1,
      design2,
      format = format,
      mode = mode,
      pager = pager,
      context = context
    ),
    class = "Diff",
    package = "diffobj"
  )
  
}

#' @rdname compare_functions
#' @importFrom diffobj diffChr
#' @export
compare_design_summaries <- function(design1, design2, format = "ansi256", mode = "sidebyside", pager = "off", context = -1L) {
  
  seed <- .Random.seed
  set.seed(seed)
  design1 <- capture.output(summary(design1))
  set.seed(seed)
  design2 <- capture.output(summary(design2))
  
  structure(
    diffChr(
      design1,
      design2,
      format = format,
      mode = mode,
      pager = pager,
      context = context
    ),
    class = "Diff",
    package = "diffobj"
  )
}

#' @rdname compare_functions
#' @importFrom diffobj diffObj
#' @export
compare_design_data <- function(design1, design2, format = "ansi256", mode = "sidebyside", pager = "off", context = -1L) {
  
  seed <- .Random.seed
  set.seed(seed)
  design1 <- draw_data(design1)
  set.seed(seed)
  design2 <- draw_data(design2)
  
  structure(
    diffObj(
      design1,
      design2,
      format = format,
      mode = mode,
      pager = pager,
      context = context
    ),
    class = "Diff",
    package = "diffobj"
  )
}

#' @rdname compare_functions
#' @importFrom diffobj diffObj
#' @export
compare_design_estimates <- function(design1, design2, format = "ansi256", mode = "sidebyside", pager = "off", context = -1L) {
  
  seed <- .Random.seed
  set.seed(seed)
  design1 <- draw_estimates(design1)
  set.seed(seed)
  design2 <- draw_estimates(design2)
  
  structure(
    diffObj(
      design1,
      design2,
      format = format,
      mode = mode,
      pager = pager,
      context = context
    ),
    class = "Diff",
    package = "diffobj"
  )
}

#' @rdname compare_functions
#' @importFrom diffobj diffObj
#' @export
compare_design_estimands <- function(design1, design2, format = "ansi256", mode = "sidebyside", pager = "off", context = -1L) {
  
  seed <- .Random.seed
  set.seed(seed)
  design1 <- draw_estimands(design1)
  set.seed(seed)
  design2 <- draw_estimands(design2)
  
  structure(
    diffObj(
      design1,
      design2,
      format = format,
      mode = mode,
      pager = pager,
      context = context
    ),
    class = "Diff",
    package = "diffobj"
  )
}

clean_call <- function(call) {
  paste(sapply(deparse(call), trimws), collapse = " ")
}

get_design_code <- function(design){
  sapply(design, function(x) clean_call(attributes(x)$call))
}

print_console_header <- function(text) {
  width <- options()$width
  cat("\n\n#", text, paste(rep("-", width - nchar(text) - 2), collapse = ""), "\n\n")
}
