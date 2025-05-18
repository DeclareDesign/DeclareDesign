
find_this_object <- DeclareDesign:::find_this_object
find_all_objects <- DeclareDesign:::find_all_objects

# case 1


test_that("magic N", {
  N <- 5
  design <- 
    declare_model(N = N, k = rnorm(N)) + NULL 
  expect_equal(nrow(draw_data(design)),5)
  rm(N)
  expect_equal(nrow(draw_data(design)),5)
  expect_true(nrow(find_all_objects(design)) == 1)
})

test_that("pars are saved", {
  n <- 5
  b <- 2
  f <- function(x, b) b*x
  design <- 
    declare_model(N = n, x = runif(N), B = b) +
    declare_model(y = f(x, b)) 
  expect_equal(nrow(draw_data(design)),5)
  rm(b)
  expect_equal(nrow(draw_data(design)),5)
  rm(f)
  expect_equal(nrow(draw_data(design)),5)
  
  expect_true(!is.null(find_this_object("b", design)))
  expect_true(length(find_this_object("b", design)) ==2)
  expect_true(length(find_this_object("f", design)) ==1)
  expect_true(!is.null(find_this_object("f", design)))
  expect_true(is.null(find_this_object("z", design)))
  
  find_all_objects(design)
  
})

test_that("n is saved", {
  n <- 5
  b <- 2
  f <- function(x, b) b*x
  step <- declare_model(N = n, x = runif(N))
  expect_true(  nrow(step()) == 5)
  rm(n)
  expect_true(  nrow(step()) == 5)
})


test_that("deeper arguments  saved", {
  n <- 5
  b <- 0
  f <- function(x) b*x
  step <- declare_model(N = n, x = runif(N), y = f(x))
  expect_true(  mean(step()$y) == 0)
  rm(b)
  expect_true(  mean(step()$y) == 0)
})


test_that("data saved", {
  ddf <- data.frame(X = runif(5))
  step <- declare_model(data = ddf)
  step()
  rm(ddf)
  expect_true(nrow(step()) == 5)
})


# edge case: 
test_that("data called 'df' (or other function name)", {
  df <- data.frame(X = runif(5))
  step <- declare_model(data = df)
  step()
  rm(df)
  expect_true(nrow(step()) == 5)
})

test_that("estimator steps", {
  n <- 100
  b <- B <- .2

  d <- 
    declare_model(N = n, Y = runif(N)) + 
    declare_estimator(Y ~ 1, subset = Y < b)
  
  expect_true(draw_estimates(d)$estimate < B)

  rm(b)

  expect_true(draw_estimates(d)$estimate < B)
})

test_that("multiple steps", {
  n <- 1000
  b <- .2
  
  d <- 
    declare_model(N = n, Y = rnorm(N, b)) + 
    declare_estimator(Y ~ 1) +
    declare_inquiry(Q = b)
  
  expect_true(run_design(d)$estimand == .2)
  
  rm(b)
  
  expect_true(run_design(d)$estimand == .2)

    d <- redesign(d, b = .4)
    x <- run_design(d)
    expect_true(x$estimand == .4)
    expect_true(abs(x$estimand - x$estimate) < .1)

    d <- redesign(d, b = 10)
    x <- run_design(d)
    expect_true(x$estimand == 10)
    expect_true(abs(x$estimand - x$estimate) < .1)
    
})


test_that("custom estimator", {

  my_estimator <- function(data) {
    data.frame(estimate = mean(data$Y))
  }


  design <-
    declare_model(
      N = 500,
      Y = rnorm(N, sd = 0.25)
    ) +
    declare_inquiry(Y_bar = mean(Y)) +
    declare_estimator(handler = label_estimator(my_estimator),
                      label = "mean",
                      inquiry = "Y_bar")
  
  expect_true(nrow(run_design(design)) ==1)
  
  rm(my_estimator)

  expect_true(nrow(run_design(design)) ==1)


})



test_that("potential outcomes environment", {
  
  a <- .2
  b <- 2
  m <- 
    declare_model(
      N = 5,
      U = rnorm(N, sd = a),
      potential_outcomes(Y ~ b)
      )
  
  expect_true(nrow(m()) ==5)
  
  expect_true(environment(environment(m)$dots$U)$a ==a)
  expect_true(environment(environment(m)$dots[[4]])$b ==b)
  rm(a,b)
  expect_true(nrow(m()) ==5)
  
})



test_that("check not overriding pipe", {
  
  U <- 1:5
  m <- 
    declare_model(
      N = 5,
      U = rnorm(N),
      Y = U
    )

  # Global U is scooped up but not actually required  or used
  expect_true(all(environment(environment(m)$dots$Y)$U == U))
  
  expect_true(m()$Y[1] !=1)
  
})


# Issue here
test_that("check not overriding pipe", {
  
n1 <- 3
n2 <- 4

m <-   
  declare_model(
    classrooms = add_level(n1),
    individuals = add_level(n2)
  ) 

rm(n1, n2)

expect_true(m() |> nrow() ==12)


})


# Design 16.1
test_that("Design 16.1", {
  
  library(rdss) # for helper functions
  library(CausalQueries)
  
  causal_model <- make_model("X -> M -> Y <- W -> M") |>
    set_restrictions("(M[X=1] < M[X=0]) | (M[X=1, W=1] == M[X=0, W=1])") |>
    set_restrictions("(Y[M=1] < Y[M=0]) | (Y[M=1, W=1] == Y[M=0, W=1])")
  
  strategies = c("X-Y", "X-Y-M", "X-Y-W",  "X-Y-W-M")
  

  declaration_16.1 <-
    declare_model(draw_causal_type(causal_model)) +
    declare_inquiry(
      CoE =  query_distribution(
        causal_model, 
        query = "Y[X=1] - Y[X=0]", 
        parameters = causal_type)) +
    declare_measurement(
      handler = function(data)
        causal_model |>
        make_data(parameters = data$causal_type))  +
    declare_estimator(
      handler = label_estimator(process_tracing_estimator), 
      causal_model = causal_model,
      query = "Y[X=1] - Y[X=0]",
      strategies = strategies)
  
  rm(causal_model, strategies)
  
  expect_true(nrow(draw_data(declaration_16.1)) == 1)
  
})



# Design 16.5
test_that("Design 16.5", {
  
  library(rdss) # for helper functions
  library(rdrobust)
  
  cutoff <- 0.5
  control <- function(X) {
    as.vector(poly(X - cutoff, 4, raw = TRUE) %*% c(.7, -.8, .5, 1))}
  treatment <- function(X) {
    as.vector(poly(X - cutoff, 4, raw = TRUE) %*% c(0, -1.5, .5, .8)) + .15}
  
  declaration_16.5 <-
    declare_model(
      N = 500,
      U = rnorm(N, 0, 0.1),
      X = runif(N, 0, 1) + U,
      D = 1 * (X > cutoff),
      Y_D_0 = control(X) + U,
      Y_D_1 = treatment(X) + U
    ) +
    declare_inquiry(LATE = treatment(cutoff) - control(cutoff)) +
    declare_measurement(Y = reveal_outcomes(Y ~ D)) +
    declare_estimator(
      Y, X, c = cutoff,
      term = "Bias-Corrected",
      .method = rdrobust_helper,
      inquiry = "LATE",
      label = "optimal"
    )  

  rm(cutoff, control, treatment)
  
  expect_true(nrow(draw_data(declaration_16.5)) == 500)
  
})




# Design library
# Obj must be available outside test environemnt for test to work
control_mean <- -100

test_that("Design library", {
    design <- DesignLibrary::two_arm_covariate_designer(control_mean = control_mean)
#   rm(control_mean)  # design has already been constructed with control_mean as object
    expect_true(draw_data(design) |> filter(Z == 0) |> pull(Y) |> mean() < -10)
    expect_true(draw_data(design |> redesign(control_mean = 100)) |> 
                  filter(Z == 0) |> pull(Y) |> mean() > 10)
    })

# Obj must be available outside test environment for test to work
n  = 10

test_that("Design library", {
 
    n = 10 
    design <- DesignLibrary::two_arm_covariate_designer(N = n)
    rm(n)
    expect_true(draw_data(design) |> nrow() == 10)

    expect_true(draw_data(design |> redesign(N = 20)) |> nrow() == 20)
    
    # find_this_object("estimand", design)  
    
    # find_all_objects(design)
    
})



# Inside function

test_that("parameter assigned in function", {
  
  designer <- function(n = 1) 
      declare_model(N = n) + NULL 
  
  expect_true(nrow(draw_data(designer(3))) == 3)
  
  designer <- function() {
    n = 4
    d <- declare_model(N = n) + NULL
    rm(n)
    d
  }

  find_this_object("n", designer())[[1]]$value ==4

  expect_true(nrow(draw_data(designer())) == 4)
  
})

test_that("runif not saved", {
  n <- 5
  design <- 
    declare_model(N = n, x = runif(N)) + NULL
  expect_false("runif" %in% find_all_objects(design)$name)
  
})


