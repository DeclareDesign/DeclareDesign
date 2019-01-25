#' Explore your design
#'
#' @param design A design object, typically created using the + operator
#'
#' @examples
#'
#' design <-
#'   declare_population(N = 500, noise = rnorm(N)) +
#'   declare_potential_outcomes(Y ~ noise + Z * rnorm(N, 2, 2)) +
#'   declare_sampling(n = 250) +
#'   declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
#'   declare_step(dplyr::mutate, noise_sq = noise^2) +
#'   declare_assignment(m = 25) +
#'   declare_reveal() +
#'   declare_estimator(Y ~ Z, estimand = "my_estimand")
#'
#' design
#'
#' df <- draw_data(design)
#'
#' estimates <- draw_estimates(design)
#' estimands <- draw_estimands(design)
#'
#' @name post_design
NULL


# For fan-out execution, convert the vector representation to (end, n) pairs

check_sims <- function(design, sims) {
  n <- length(design)
  if (!is.data.frame(sims)) {
    if (length(sims) == n) {
      sims_full <- sims
    }
    else if (is.character(names(sims))) {
      sims_full <- rep(1, n)
      design_labels <- as.character(lapply(design, attr, "label"))
      i <- match(names(sims), design_labels)
      sims_full[i] <- sims
    } else if (length(sims) != n) {
      sims_full <- c(sims, rep(1, n))[1:n]
    }

    ret <- data.frame(end = 1:n, n = sims_full)
  }

  # Compress sequences of ones into one partial execution
  
  if(n > 1) {
    j <- 1
    for(i in 2:n){
      k <- ret[i, "n"]
      if(k > 1) {
        #keeper
        j <- j + 1
        ret[j,] <- c(i,k) 
      } else if(k == 1) {
        ret[j, "end"] <- i
      }
    }
    ret <- ret[1:j, , drop=FALSE]
  }
  
  ret
}

#' Execute a design
#'
#' @param design a DeclareDesign object
#'
#' @export
run_design <- function(design) run_design_internal(design)

run_design_internal <- function(design, ...) UseMethod("run_design_internal", design)

next_step <- function(step, current_df, i) {
  tryCatch(
    nxt <- step(current_df),
    error = function(err) {
      stop(simpleError(sprintf("Error in step %d (%s):\n\t%s", i, attr(step, "label") %||% "", err)))
    }
  )
  nxt
}

run_design_internal.default <- function(design) {
  stop("Please only send design objects or functions with no arguments to run_design.")
}

run_design_internal.design <- function(design, current_df = NULL, results = NULL, start = 1, end = length(design), ...) {
  if (!is.list(results)) {
    results <- list(
      estimand = vector("list", length(design)),
      estimator = vector("list", length(design))
    )
  }

  for (i in seq(start, end)) {
    step <- design[[i]]

    causal_type <- attr(step, "causal_type")
    step_type <- attr(step, "step_type")

    # if it's a dgp
    if ("dgp" %in% causal_type) {
      current_df <- next_step(step, current_df, i)
    } else if (step_type %in% names(results)) {
      results[[step_type]][[i]] <- next_step(step, current_df, i)
    } else {
      NULL # skipping steps not in the requested results types
    }
  }

  if (i == length(design)) {
    if ("estimator" %in% names(results)) {
      results[["estimates_df"]] <- rbind_disjoint(results[["estimator"]])
      results[["estimator"]] <- NULL
    }
    if ("estimand" %in% names(results)) {
      results[["estimands_df"]] <- rbind_disjoint(results[["estimand"]])
      results[["estimand"]] <- NULL
    }
    if ("current_df" %in% names(results)) {
      results[["current_df"]] <- current_df
    }
    append(results, list(...))
    
  } else {
    execution_st(
      design = design,
      current_df = current_df,
      results = results,
      start = i + 1,
      end = length(design),
      ...
    )
  }
}

# for when the user sends a function that runs a design itself
#   to run_design (or simulate_design / diagnose_design above it)
run_design_internal.function <- function(design) {
  design()
}

run_design_internal.execution_st <- function(design, ...) do.call(run_design_internal.design, design)

# Build an execution strategy object
#
# @param design a design
# @param current_df a data.frame
# @param results a list of intermediate results
# @param start index of starting step
# @param end  index of ending step
execution_st <- function(design, current_df = NULL, results = NULL, start = 1, end = length(design), ...) {
  # An execution state are the arguments needed to run run_design
  structure(
    list(
      design = design,
      current_df = current_df,
      results = results,
      start = start,
      end = end,
      ...
    ),
    class = "execution_st"
  )
}


apply_on_design_dots <- function(FUN, ...) {
  designs <- dots_to_list_of_designs(...)

  elist <- lapply(designs, FUN)

  if (length(designs) > 1) {
    elist <- Map(cbind, design_label = names(elist), elist, stringsAsFactors = FALSE)
  }

  rbind_disjoint(elist)
}

dots_to_list_of_designs <- function(...) {
  dotqs <- enquos(...)
  
  if (length(dotqs) == 0){
    stop("Please provide at least one design.", call. = FALSE)
  }
  
  d1 <- eval_tidy(dotqs[[1]])

  ## Two cases:
  ## 1. send one or more design objects created by the + operator
  ## 2. send a single list of design objects e.g. created by expand_design
  ## Approach: unpack designs if a list of designs was sent as a single list object
  if (length(dotqs) == 1 &&
    is.list(d1) &&
    !inherits(d1, "design")) {
    designs <- d1
    names(designs) <- infer_names(designs)
  } else {
    names(dotqs) <- infer_names(dotqs)
    designs <- eval_tidy(quo(list(!!!dotqs)))
  }

  # do not allow users to send more than one object if any is not a design object
  check_design_class(designs)

  designs
}


#' Print code to recreate a design
#'
#' @examples
#'
#' my_population <- declare_population(N = 100)
#'
#' my_assignment <- declare_assignment(m = 50)
#'
#' my_design <- my_population + my_assignment
#'
#' print_code(my_design)
#'
#' @rdname post_design
#'
#' @export
print_code <- function(design) {
  
  # if there is not a code attribute, construct code via the calls for each step
  #   and the call for the declare step
  
  if (is.null(attributes(design)$code)) {
    clean_call <- function(call) {
      paste(sapply(deparse(call), trimws), collapse = " ")
    }
    
    # print each step
    
    for (i in seq_along(design)) {
      # only print steps that are not calls within the design call i.e. mutate(q = 5)
      if (inherits(attributes(design[[i]])$call, "call")) {
        cat(names(design)[i], "<-", clean_call(attributes(design[[i]])$call), "\n\n")
      }
    }
    
    # print the design declaration
    
    cat("my_design <-", clean_call(attributes(design)$call), "\n\n")
  } else {
    print(attributes(design)$code)
  }
}

#' Obtain the preferred citation for a design
#'
#' @param design a design object created using the + operator
#'
#' @param ... options for printing the citation if it is a BibTeX entry
#'
#' @export
cite_design <- function(design, ...) {
  citation <- attr(design, "citation")
  if (class(citation) == "bibentry") {
    print(citation, style = "bibtex", ... = ...)
  } else {
    print(citation, style = "text", ... = ...)
  }
}

#' @export
print.design_step <- function(x, ...) {
  print(attr(x, "call"))
}

#' @export
str.design_step <- function(object, ...) cat("design_step:\t", paste0(deparse(attr(object, "call"), width.cutoff = 500L), collapse = ""), "\n")

make_fan_counter <- function(fan) {
  k <- nrow(fan)
  ret <- matrix(0, 1, k)
  colnames(ret) <- sprintf("step_%d_draw", c(1, fan$end+1)[1:k])
  
  ret
}

# A wrapper around conduct design for fan-out execution strategies
fan_out <- function(design, fan) {
  st <- list(execution_st(design, fan=make_fan_counter(fan)))

  for (i in seq_len(nrow(fan))) {
    end <- fan[i, "end"]
    n <- fan[i, "n"]

    for (j in seq_along(st))
      st[[j]]$end <- end

    st <- st [ rep(seq_along(st), each = n) ]

    for (j in seq_along(st))
      st[[j]]$fan[i] <- j
    
    
    st <- future_lapply(seq_along(st), function(j) run_design(st[[j]]), future.seed = NA, future.globals = "st")
  }
  
  st <- lapply(st, function(x){
    fan <- x$fan
    x$fan <- NULL
    lapply(x, function(x, z=nrow(x)) if(z > 0) cbind(x,fan) else x)
  })
  

  st
}
