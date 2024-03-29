

context("functions in designers")

test_that("simple designer works", {
  my_designer <- function(N = 100,
                            my_inquiry_func = mean) {
    my_pop <- declare_model(N = N, Y = rnorm(N))
    my_inquiry <- declare_inquiry(inquiry = my_inquiry_func(Y))
    my_design <- my_pop + my_inquiry
    my_design
  }

  expect_length(design_list <-
    expand_design(
      designer = my_designer,
      N = c(c(20, 20, 20), c(20, 20, 20), c(20, 20, 20)),
      my_inquiry_func = c(mean, median)
    ), 18)

  expect_length(design_list <-
    expand_design(
      designer = my_designer,
      N = c(10, 50, 100),
      my_inquiry_func = c(mean, median)
    ), 6)

  expect_length(design_list <-
    expand_design(
      designer = my_designer,
      N = c(10, 50, 100),
      my_inquiry_func = list(mean, median)
    ), 6)


  expect_length(design_list <-
    expand_design(
      designer = my_designer,
      N = c(10, 50, 100),
      my_inquiry_func = mean
    ), 3)
})


test_that("designer with vector argument works", {
  my_designer <- function(N = c(100, 50),
                            my_inquiry_func = mean) {
    my_pop <- declare_model(N = min(N), Y = rnorm(N))
    my_inquiry <- declare_inquiry(inquiry = my_inquiry_func(Y))
    my_design <- my_pop + my_inquiry
    my_design
  }

  expect_length(design_list <-
    expand_design(
      designer = my_designer,
      N = list(c(20, 20, 20), c(20, 20, 20), c(20, 20, 20)),
      my_inquiry_func = c(mean, median)
    ), 6)
})




context("functions in designers")

my_designer <- function(N = 100,
                        my_inquiry_func = mean) {
  my_pop <- declare_model(N = N, Y = rnorm(N))
  my_inquiry <- declare_inquiry(inquiry = my_inquiry_func(Y))
  my_design <- my_pop + my_inquiry
  my_design
}


test_that("expand_design works", {
  design_list <-
    expand_design(
      designer = my_designer,
      N = c(c(20, 20, 20), c(20, 20, 20), c(20, 20, 20)),
      my_inquiry_func = c(mean, median)
    )
  diag <- diagnose_design(design_list, sims = 5, bootstrap_sims = FALSE)
  expect_true(all(c("N", "my_inquiry_func") %in% names(diag$diagnosands_df)))

  design_list <-
    expand_design(
      designer = my_designer,
      N = c(10, 50, 100),
      my_inquiry_func = c(mean, median)
    )
  diag <- diagnose_design(design_list, sims = 5, bootstrap_sims = FALSE)
  expect_true(all(c("N", "my_inquiry_func") %in% names(diag$diagnosands_df)))

  design_list <-
    expand_design(
      designer = my_designer,
      N = c(10, 50, 100),
      my_inquiry_func = list(mean, median)
    )
  diag <- diagnose_design(design_list, sims = 5, bootstrap_sims = FALSE)
  expect_true(all(c("N", "my_inquiry_func") %in% names(diag$diagnosands_df)))


  design_list <-
    expand_design(
      designer = my_designer,
      N = c(10, 50, 100),
      my_inquiry_func = mean
    )

  diag <- diagnose_design(design_list, sims = 5, bootstrap_sims = FALSE)
  expect_true(all(c("N", "my_inquiry_func") %in% names(diag$diagnosands_df)))
})


test_that("even more kinds of parameters can be sent, vectors and scalars, etc.", {
  my_designer <- function(N, ate) {
    pop <- declare_model(N = N, noise = rnorm(N))
    pos <- declare_model(Y ~ ate * Z + noise)
    assgn <- declare_assignment(Z = complete_ra(N, m = N / 2))
    inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
    estimator <- declare_estimator(Y ~ Z, inquiry = inquiry)
    pop + pos + assgn + inquiry + estimator
  }

  designs <- expand_design(my_designer, N = seq(30, 100, 10), ate = seq(0, .5, length.out = 3))
  expect_equal(sapply(designs, attr, "parameters"), structure(list(
    "30", "0", "40", "0", "50", "0", "60", "0", "70",
    "0", "80", "0", "90", "0", "100", "0", "30", "0.25", "40",
    "0.25", "50", "0.25", "60", "0.25", "70", "0.25", "80", "0.25",
    "90", "0.25", "100", "0.25", "30", "0.5", "40", "0.5", "50",
    "0.5", "60", "0.5", "70", "0.5", "80", "0.5", "90", "0.5",
    "100", "0.5"
  ), .Dim = c(2L, 24L), .Dimnames = list(c(
    "N",
    "ate"
  ), c(
    "design_1", "design_2", "design_3", "design_4", "design_5",
    "design_6", "design_7", "design_8", "design_9", "design_10",
    "design_11", "design_12", "design_13", "design_14", "design_15",
    "design_16", "design_17", "design_18", "design_19", "design_20",
    "design_21", "design_22", "design_23", "design_24"
  ))))


  my_designer <- function(N = 100,
                            my_inquiry_func = mean) {
    my_pop <- declare_model(N = N, Y = rnorm(N))
    my_inquiry <- declare_inquiry(inquiry = my_inquiry_func(Y))
    my_design <- my_pop + my_inquiry
    my_design
  }

  design_list <-
    expand_design(
      designer = my_designer,
      N = c(10, 50, 100),
      my_inquiry_func = c(mean, median)
    )

  expect_equal(sapply(design_list, attr, "parameters"), structure(list(
    "10", "mean", "50", "mean", "100", "mean", "10",
    "median", "50", "median", "100", "median"
  ), .Dim = c(
    2L,
    6L
  ), .Dimnames = list(c("N", "my_inquiry_func"), c(
    "design_1",
    "design_2", "design_3", "design_4", "design_5", "design_6"
  ))))

  design_list <-
    expand_design(
      designer = my_designer,
      N = c(10, 50, 100),
      my_inquiry_func = 10
    )

  expect_equal(sapply(design_list, attr, "parameters"), structure(list("10", "10", "50", "10", "100", "10"), .Dim = 2:3, .Dimnames = list(
    c("N", "my_inquiry_func"), c("design_1", "design_2", "design_3")
  )))

  design_list <-
    expand_design(
      designer = my_designer,
      N = c(10, 50, 100),
      my_inquiry_func = mean
    )

  expect_equal(sapply(design_list, attr, "parameters"), structure(list("10", "mean", "50", "mean", "100", "mean"), .Dim = 2:3, .Dimnames = list(
    c("N", "my_inquiry_func"), c("design_1", "design_2", "design_3")
  )))
})


test_that("edge case with expand but one arg works", {
  
  my_designer <- function(N = 100,
                          my_inquiry_func = mean) {
    my_pop <- declare_model(N = N, Y = rnorm(N))
    my_inquiry <- declare_inquiry(inquiry = my_inquiry_func(Y))
    my_design <- my_pop + my_inquiry
    my_design
  }
  
  expect_length(expand_design(
                    designer = my_designer, N = 5), 2)
  expect_length(expand_design(
    designer = my_designer, N = 5, expand = FALSE), 2)
})

test_that("expand with vector arguments", {
  
  my_designer <- function(N=10, z = list(1,5,9)) {
    my_pop <- declare_model(top=add_level(N = length(z), z=unlist(z)), 
                                 bottom=add_level(N=z, Y = rnorm(N)))
    my_inquiry <- declare_inquiry(inquiry = max(table(top)))
    my_design <- my_pop + my_inquiry
    my_design
  }
  
  expect_equal(
    draw_estimands(expand_design(my_designer, z=list(2)))$estimand, 2)
  
  
  zx <- list(1:4, 2:10, 9:1, list(4,9,2))
  
  dsns <- expand_design(my_designer, z=zx)
  
  expect_equivalent(
    unlist(sapply(dsns, draw_estimands)[2,]),
    sapply(zx, function(x)max(unlist(x)))
  )
  
})
