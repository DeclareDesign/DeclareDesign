

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
  
})


test_that("all steps", {
  

  b1 <- 1
  b2 <- 2
  b3 <- 3
  b4 <- 4
  b5 <- 5
  b6 <- 6
  b7 <- 7
  b8 <- 8
  
  design <- 
    declare_model(N = 20, Y = rnorm(N), B = b1) +
    declare_inquiry(Q = b2) +
    declare_assignment(Z = simple_ra(N, prob = b3/b3)) +
    declare_potential_outcomes(Y ~ Z + b4) +
    declare_sampling(S = complete_rs(N = N, n = b5)) +
    declare_measurement(K = b6) +
    declare_estimator(Y ~ 1, subset = Y < b7) +
    declare_model(D = 1)
  
  rm(b1, b2, b3, b4, b5, b6, b7, b8)
  
  x <- find_all_objects(design)
  x
  # appears in every step (step 4 not yet functioning)
  expect_true(all(c(1,2,3,5,6,7) %in% x$step))
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

test_that("find object after redesign", {
  n <- 5
  b <- 2
  r <- 2
  f <- function(x, b) b*x + r
  design <- declare_model(N = n, x = runif(N), w = f(x, b), s = 2*w) + NULL
  rm(n,b,r, f)
  # needs to find r
  DeclareDesign:::find_all_objects(design)
  
  # expect_warning(DeclareDesign:::modify_edit(design, w = 3))
  
  design <- DeclareDesign:::modify_edit(design, n = 3)
  expect_true(draw_data(design) |> nrow() ==3)
  DeclareDesign:::find_all_objects(design)
  
  design <- redesign(design, n = 7)
  expect_true(draw_data(design) |> nrow() ==7)
  
#  DeclareDesign:::find_all_objects(design)
})


test_that("formula OK", {
  b = 2; n  = 2
  step <- declare_model(N = n, potential_outcomes( Y ~ b*Z))
  rm(b)
  expect_true( all(step()[1, 2:3] == c(0,2)))
})


test_that("potential outcomes OK", {
  b = 2
  d <- declare_model(N = 2, B = b) + declare_potential_outcomes( Y ~ b*Z)
  rm(b)
  expect_true(all( draw_data(d)[1, 3:4] == c(0,2)))
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
  find_all_objects(step + NULL)
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

  expect_true(nrow(draw_data(designer())) == 4)
  
})

test_that("runif not saved", {
  n <- 5
  design <- 
    declare_model(N = n, x = runif(N)) + NULL
  expect_false("runif" %in% DeclareDesign:::find_all_objects(design)$name)
  
})


test_that("functions saved", {
  
  f <- runif
  n <- 5
  design <- 
    declare_model(N = n, x = f(N)) + NULL
  expect_true("f" %in% DeclareDesign:::find_all_objects(design)$name)
  
})

test_that("environment sharing", {
  N <- 5
  design <- 
    declare_model(N = N, x = runif(N)) + NULL

  design_2 <- DeclareDesign:::modify_edit(design, N = 6)
  design_3 <- DeclareDesign:::modify_edit(design, N = 7)
  
  find_all_objects(design_2)
  find_all_objects(design_3)
  
  expect_false("runif" %in% find_all_objects(design)$name)
  
})




# Test with formula


test_that("param in po formula quosure", {
  N <- 2
  b <- .2
  design <- 
    declare_model(N = N, U = rnorm(N)) + 
    declare_potential_outcomes(Y ~ b*N*Z)
  rm(N, b)
  draw_data(design)

  obs <- DeclareDesign:::find_all_objects(design)
  
  expect_true(all(obs$name == c("N", "b")))
  
  design <- redesign(design, N = 4, b =.1)
  
  expect_true(all(DeclareDesign:::find_all_objects(design) |> dplyr::pull(value_str) == c(4, .1)))

})


# Currently failing (saving OK, but recovery not)
# to do: remove N from handlr environment

test_that("param in handler", {
  N <- 2
  b <- .2
  f <- function(...) fabricate(...)
  hdl <- function(...) f(..., extra = rnorm(N, b))

  hdl
  ls(environment(hdl))
  hdl <- DeclareDesign:::capture_function_dependencies(hdl)
  rm(N, b, f)
  
  expect_true(all(ls(environment(hdl)) == c("b", "f")))
})



test_that("behavior when packaged used and removed", {
  
  library(CausalQueries)
  model_handler <- function(N) make_model() |> make_data(N)
  n <- 2
  
  design <- 
    declare_model(handler = model_handler, N = n) +  NULL

  rm(n)
  
  obs <- DeclareDesign:::find_all_objects(design)
  obs
  
  expect_true(nrow(draw_data(design)) ==2)
  
  detach("package:CausalQueries", unload = TRUE)

  # Object can be inspected
  expect_error(DeclareDesign:::find_all_objects(design), NA)
  
  # But does not run without a path to the functions used
  expect_error(draw_data(design))
  
})



test_that("variables confused for arguments", {
  
n <- 1
  design <- 
    declare_model(N = n, A = 1) +
    declare_model(B = A) +
    declare_potential_outcomes(Y ~ Z + A) 
  
  expect_true(DeclareDesign:::find_all_objects(design) |> nrow() ==1)
  
  n <- 1
  step_1 <-  declare_model(N = n, A = 1) 
  step_2 <-  declare_model(B = A) 
  step_3 <-  declare_potential_outcomes(Y ~ Z + A) 
  design <- step_1 + step_2 + step_3
    
  expect_true(DeclareDesign:::find_all_objects(design) |> nrow() ==1)
  
  
})


test_that("declare_population handles environments OK", {
  
  # 1 simple
  
  a <- 1 

  design <-
    declare_population(N = 5, u_1 = rnorm(N), 
                       u_2 = rnorm(N)) +
    declare_potential_outcomes(formula = Y ~ a)  
  rm(a)
  expect_true(nrow(draw_data(design)) == 5)

  # 2 with conditions
  
  a <- 1.1 
  
  design <-
    declare_population(N = 5, u_1 = rnorm(N), 
                       u_2 = rnorm(N)) +
    declare_potential_outcomes(formula = Y ~ a + (Z=="1") + rnorm(N)/100, 
                               conditions = list(Z = c("1", "2")))  
  
  rm(a)
  expect_true(sd(draw_data(design)$Y_Z_1) != 0)
  
  a <- 1.12 
  design <-
    declare_population(N = 5, u_1 = 1.3, 
                       u_2 = rnorm(N)) +
    declare_potential_outcomes(formula = Y ~ a + u_1, 
                               conditions = list(Z = c("1", "2")))  
  
  rm(a)
  expect_true(mean(draw_data(design)$Y_Z_1) == 2.42)
  

  # sd type parameter handled properly
  sd <- 1000
  design <-
    declare_model(N = 5, u_W = rnorm(N), 
                  u_Y = rnorm(n = N, mean = .5 * u_W, sd = sqrt(1 - .5^2))) +
    declare_potential_outcomes(Y ~ (u_Y * sd ))
  design
  expect_true(sd(draw_data(design)$Y_Z_1) > 5)
  
  })
