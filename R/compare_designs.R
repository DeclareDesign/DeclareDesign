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
#'  compare_design_code(design1, design2)
#'  compare_design_summary(design1, design2)
#'  compare_design_data(design1, design2)
#'  compare_design_estimates(design1, design2)
#'  compare_design_estimands(design1, design2)
#' 
#' @name compare_functions


#' @rdname compare_functions
#' @importFrom diffobj diffChr
#' @export
compare_design_code <- function(design1, design2, format = "ansi8", mode = "sidebyside") {
  
  design1 <- get_design_code(design1)
  design2 <- get_design_code(design2)
  
  structure(
    diffChr(
      design1,
      design2,
      format = format,
      mode = mode,
      pager = "off",
      context = -1L
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


#' @rdname compare_functions
#' @importFrom diffobj diffChr
#' @export
compare_design_summary <- function(design1, design2, format = "ansi256", mode = "sidebyside") {
  
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
      pager = "off",
      context = -1L
    ),
    class = "Diff",
    package = "diffobj"
  )
}



#' @rdname compare_functions
#' @importFrom diffobj diffObj
#' @export
compare_design_data <- function(design1, design2, format = "ansi256", mode = "sidebyside") {
  
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
      pager = "off",
      context = -1L
    ),
    class = "Diff",
    package = "diffobj"
  )
}

#' @rdname compare_functions
#' @importFrom diffobj diffObj
#' @export
compare_design_estimates <- function(design1, design2, format = "ansi256", mode = "sidebyside") {
  
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
      pager = "off",
      context = -1L
    ),
    class = "Diff",
    package = "diffobj"
  )
}

#' @rdname compare_functions
#' @importFrom diffobj diffObj
#' @export
compare_design_estimands <- function(design1, design2, format = "ansi256", mode = "sidebyside") {
  
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
      pager = "off",
      context = -1L
    ),
    class = "Diff",
    package = "diffobj"
  )
}
