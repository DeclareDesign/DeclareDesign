compare_partial <- function(FUN, DIFFFUN, is_data = FALSE){
  if(is_data){
    
    function(design1,
             design2,
             format = "ansi256",
             mode = "auto",
             pager = "off",
             context = -1L,
             rmd = FALSE) {
      stopifnot(requireNamespace("diffobj"))
      DIFFFUN <- get(DIFFFUN, getNamespace("diffobj"))
      compare_design_internal(
        FUN,
        DIFFFUN,
        design1,
        design2,
        format = format,
        mode = mode,
        pager = pager,
        context = context,
        rmd = rmd
      )
    }
    
  } else{
    
    function(design1,
             design2,
             format = "ansi256",
             mode = "sidebyside",
             pager = "off",
             context = -1L,
             rmd = FALSE) {
      stopifnot(requireNamespace("diffobj"))
      DIFFFUN <- get(DIFFFUN, getNamespace("diffobj"))
      compare_design_internal(
        FUN,
        DIFFFUN,
        design1,
        design2,
        format = format,
        mode = mode,
        pager = pager,
        context = context,
        rmd = rmd
      )
    }
  }
}


#' Compare two designs
#'
#' @param design1 A design object, typically created using the + operator
#' @param design2 A design object, typically created using the + operator
#' @param format Format (in console or HTML) options from \code{diffobj::diffChr}
#' @param mode Mode options from \code{diffobj::diffChr}
#' @param pager Pager option from \code{diffobj::diffChr}
#' @param context Context option from \code{diffobj::diffChr} which sets the number of lines around differences that are printed. By default, all lines of the two objects are shown. To show only the lines that are different, set \code{context = 0}; to get one line around differences for context, set to 1.
#' @param rmd Set to \code{TRUE} use in Rmarkdown HTML output. NB: will not work with LaTeX, Word, or other .Rmd outputs.
#'
#' @examples
#' 
#' design1 <- declare_model(N = 100, u = rnorm(N), potential_outcomes(Y ~ Z + u)) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(S = complete_rs(N, n = 75)) +
#'   declare_assignment(Z = complete_ra(N, m = 50)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, inquiry = "ATE")
#' 
#'   design2 <- declare_model(N = 200, U = rnorm(N),
#'                          potential_outcomes(Y ~ 0.5*Z + U)) +
#'   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_sampling(S = complete_rs(N, n = 100)) +
#'   declare_assignment(Z = complete_ra(N, m = 25)) +
#'   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#'   declare_estimator(Y ~ Z, .method = lm_robust, inquiry = "ATE")
#'  
#'  if (require("diffobj")) {
#'  
#'    compare_designs(design1, design2)
#'    compare_design_code(design1, design2)
#'    compare_design_summaries(design1, design2)
#'    compare_design_data(design1, design2)
#'    compare_design_estimates(design1, design2)
#'    compare_design_inquiries(design1, design2)
#'  
#'  }
#' 
#' @name compare_functions


#' @rdname compare_functions
#' @export
compare_designs <- function(design1, design2, format = "ansi8", pager = "off", context = -1L, rmd = FALSE) {
  
  compare_functions <-
    list(code_comparison = compare_design_code,
         data_comparison = compare_design_data, 
         estimands_comparison = compare_design_inquiries,
         estimates_comparison = compare_design_estimates)
  
  vals <-
    lapply(compare_functions, function(fun)
      fun(
        design1,
        design2,
        format = format,
        pager = pager,
        context = context, 
        rmd = rmd
      )
    )
  
  class(vals) <- "design_comparison"
  
  vals
}

#' @export
print.design_comparison <- function(x, ...) {
  cat("Research design comparison\n\n")
  
  labels <- c("code_comparison" = "design code", 
              "data_comparison" = "draw_data(design)",
              "estimands_comparison" = "draw_estimands(design)",
              "estimates_comparison" = "draw_estimates(design)")
  
  for(n in names(labels)) {
    print_console_header(paste("Compare", labels[n]))
    print(x[[n]])
  }
  
}


#' @rdname compare_functions
#' @export
compare_design_code <- compare_partial(get_design_code, "diffObj")

#' @rdname compare_functions
#' @export
compare_design_summaries <- compare_partial(function(x) capture.output(summary(x)), "diffChr")

#' @rdname compare_functions
#' @export
compare_design_data <- compare_partial(draw_data, "diffObj")

#' @rdname compare_functions
#' @export
compare_design_estimates <- compare_partial(draw_estimates, "diffObj", is_data = TRUE)

#' @rdname compare_functions
#' @export
compare_design_inquiries <- compare_partial(draw_estimands, "diffObj", is_data = FALSE)


compare_design_internal <- function(FUN, DIFFFUN, design1, design2, format = "ansi256", mode = "sidebyside", pager = "off", context = -1L, rmd = FALSE){
  check_design_class_single(design1)
  check_design_class_single(design2)
  
  seed <- .Random.seed
  design1 <- FUN(design1)
  set.seed(seed)
  design2 <- FUN(design2)
  
  if(rmd == TRUE) {
    format <- "html"
    style <- list(html.output = "diff.w.style")
  } else {
    style <- "auto"
  }

  diff_output <- structure(
    DIFFFUN(
      design1,
      design2,
      format = format,
      mode = mode,
      pager = pager,
      context = context,
      style = style
    ),
    class = "Diff",
    package = "diffobj"
  )
  
  if(rmd == TRUE) {
    cat(as.character(diff_output))
  } else {
    diff_output
  }
  
}

clean_call <- function(call) {
  paste(sapply(deparse(call), trimws), collapse = " ")
}

get_design_code <- function(design){
  if (is.null(attributes(design)$code)) {
    sapply(design, function(x) clean_call(attr(x, "call")))
  } else {
    attributes(design)$code
  }
}

print_console_header <- function(text) {
  width <- options()$width
  cat("\n\n#", text, paste(rep("-", width - nchar(text) - 2), collapse = ""), "\n\n")
}


