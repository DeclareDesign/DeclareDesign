

test_that("custom po handler", {

  # draw POs for it without arguments
  my_po_function <- function(data) {
    data$Y_Z_0 <- with(data, .25 + extra)
    data$Y_Z_1 <- with(data, extra)
    data
  }

  ## debugonce(declare_potential_outcomes)
  my_po_custom <- declare_potential_outcomes(handler = my_po_function)

  rm(my_po_function)
  pop_custom <- my_po_custom(sleep)

  expect_equal(colnames(pop_custom), c("extra", "group", "ID", "Y_Z_0", "Y_Z_1"))
})

test_that("custom po handler with args", {

  ## draw POs for it with arguments

  my_po_function <- function(data, q) {
    data$Y_Z_0 <- with(data, q + extra)
    data$Y_Z_1 <- with(data, extra)
    data
  }

  ## debugonce(declare_potential_outcomes)
  my_po_custom <- declare_potential_outcomes(
    handler = my_po_function, q = 2
  )

  ## debugonce(my_po_custom)
  rm(my_po_function)
  pop_custom <- my_po_custom(sleep)

  expect_equal(colnames(pop_custom), c("extra", "group", "ID", "Y_Z_0", "Y_Z_1"))
  expect_equal(pop_custom$Y_Z_1[1] - pop_custom$Y_Z_0[1], -2)
})




test_that("PO as discrete variables works", {

  extra <- 1
  
  my_potential_outcomes <- declare_potential_outcomes(
    Y_Z_0 = extra,
    Y_Z_1 = extra + 5
  )

  expect_equal(
    colnames(my_potential_outcomes(sleep)),
    c("extra", "group", "ID", "Y_Z_0", "Y_Z_1")
  )
})


# to: remove this N after capturing pars in

test_that("PO as a formula works", {
  N <- 3
  data <- fabricate(N = N)
  
  my_potential_outcomes_explicit <-
    declare_potential_outcomes(
      formula = R ~ rbinom(n = N, size = 1, prob = 1))
  
  my_potential_outcomes_implicit <-
    declare_potential_outcomes(R ~ rbinom(n = N, size = 1, prob = 1))
  
  expect_identical(
    my_potential_outcomes_explicit(data), #
    my_potential_outcomes_implicit(data)  # OK
  )
})

set.seed(5)
my_population <- declare_model(
  villages = add_level(N = 3, elevation = rnorm(N)),
  citizens = add_level(N = 4, income = runif(N))
)


# levels approach no longer working
test_that("POs at a higher level", {
  library(dplyr)
  my_population <- declare_model(
    villages = add_level(N = 3, elevation = rnorm(N)),
    citizens = add_level(N = 4, income = runif(N))
  )

  pop <- my_population()

  # different ways of doing the same thing

  # with "level" argument in a "formula" version

    my_potential_outcomes_formula <-
    declare_potential_outcomes(
      formula = Y_vil ~ elevation + 5 + 2 * Z,
      level = "villages"
    )
  my_potential_outcomes_formula(pop)



  # with "level" argument in a "discrete" version
  my_potential_outcomes_discrete <-
    declare_potential_outcomes(
      Y_vil_Z_0 = elevation + 5,
      Y_vil_Z_1 = elevation + 5 + 2,
      level = "villages"
    )

  expect_equal(
    my_potential_outcomes_discrete(pop) |> head(),
    structure(list(villages = c("1", "1", "1", "1", "2", "2"), elevation = c(-0.840855480786298, 
                                                                             -0.840855480786298, -0.840855480786298, -0.840855480786298, 1.38435934347858, 
                                                                             1.38435934347858), citizens = c("01", "02", "03", "04", "05", 
                                                                                                             "06"), income = c(0.527959984261543, 0.807935200864449, 0.9565001251176, 
                                                                                                                               0.110453018685803, 0.273284949595109, 0.490513201802969), Y_vil_Z_0 = c(4.1591445192137, 
                                                                                                                                                                                                       4.1591445192137, 4.1591445192137, 4.1591445192137, 6.38435934347858, 
                                                                                                                                                                                                       6.38435934347858), Y_vil_Z_1 = c(6.1591445192137, 6.1591445192137, 
                                                                                                                                                                                                                                        6.1591445192137, 6.1591445192137, 8.38435934347858, 8.38435934347858
                                                                                                                                                                                                       )), row.names = c(NA, 6L), class = "data.frame")
  )
  
})

test_that("pos at a higher level with dplyr", {
  
  skip_if_not_installed("dplyr")
  library(dplyr)
  
  # with custom function
  my_custom_PO <- function(data) {
    data |>
      group_by(villages) |>
      mutate(
        Y_vil_Z_0 = elevation + 5,
        Y_vil_Z_1 = elevation + 5 + 2
      )
  }


  my_custom_PO(pop)

  my_potential_outcomes <-
    declare_potential_outcomes(
      formula = Y_vil ~ elevation + 5 + 2 * Z
    )

  my_design <-
    declare_model(data = pop) +
    declare_step(group_by, villages) +
    my_potential_outcomes

  my_design <-
    declare_model(data = pop) +
    declare_step(group_by, villages) +
    my_potential_outcomes

  expect_equal(nrow(draw_data(my_design)), 12)
})


test_that("draw POs at a level using a variable from another level (now allowed)", {
  my_population <- declare_model(
    villages = add_level(N = 2, elevation = 1:2),
    citizens = add_level(N = 2, income = c(.1, .3))
  )

  pop <- my_population()

  my_potential_outcomes_formula <-
    declare_potential_outcomes(
      formula = Y_vil ~ elevation + income + 5,
      level = "villages"
    )

  expect_equivalent(my_potential_outcomes_formula(pop)$Y_vil_Z_1,
                    c(6.1, 6.3, 7.1, 7.3))
  })


test_that("Potential outcomes with multiple assignment variables", {
  extra = 2
  beta <- c(1, 3)

  my_potential_outcomes_formula <-
    declare_potential_outcomes(
      formula = test ~ extra + as.vector((cbind(z1, z2) %*% beta)),
      conditions = list(z1 = 0:1, z2 = 1:2)
    )
  
  out <- my_potential_outcomes_formula(sleep)
  
  with(out, {
    expect_equal(extra + 3, test_z1_0_z2_1)
    expect_equal(extra + 4, test_z1_1_z2_1)
    expect_equal(extra + 6, test_z1_0_z2_2)
    expect_equal(extra + 7, test_z1_1_z2_2)
  })

  # Assignment variables handled as conditions
  my_potential_outcomes_formula <-
    declare_potential_outcomes(
      formula = test ~ extra + z1 + z2,
      assignment_variables = c("z1", "z2")
    )
  out <- my_potential_outcomes_formula(sleep)
  
  expect_true(my_potential_outcomes_formula(sleep) |> ncol() == 7)
  
  
  # my_potential_outcomes_formula <-
  #   declare_potential_outcomes(
  #     formula = test ~ extra + as.vector((cbind(z1, z2) %*% beta)),
  #     assignment_variables = c("z1", "z2")
  #   )
  # out <- my_potential_outcomes_formula(sleep)
  # with(out, {
  #   expect_equal(extra, test_z1_0_z2_0)
  #   expect_equal(extra + 3, test_z1_0_z2_1)
  #   expect_equal(extra + 1, test_z1_1_z2_0)
  #   expect_equal(extra + 4, test_z1_1_z2_1)
  #
})




test_that("Restore existing variables to be unchanged", {
  my_potential_outcomes_formula <-
    declare_potential_outcomes(
      formula = test ~ extra + group,
      conditions = list(group = 1:2)
    )
  expect_identical(
    my_potential_outcomes_formula(sleep)$group,
    sleep$group
  )
})




test_that("Binary Potential outcomes", {
  my_potential_outcomes_formula <-
    declare_potential_outcomes(
      Y ~ draw_binary(prob = plogis(1000 * Z + extra))
    )

  out <- my_potential_outcomes_formula(sleep)
  expect_true(all(out$Y_Z_1 == 1))
})


test_that("Multiple assignment variables in PO", {
  po <- declare_potential_outcomes(Y ~ Z1 + Z2, conditions = list(Z1 = 0:1, Z2 = 0:1))
  df <- po(sleep)
  expect_true(all(c("Y_Z1_0_Z2_0", "Y_Z1_1_Z2_0", "Y_Z1_0_Z2_1", "Y_Z1_1_Z2_1") %in%
               names(df)))
})


test_that("handler dispatches correctly", {
  expect_error(
    potential_outcomes_handler(
      Y ~ Z1 + Z2,
      conditions = expand.grid(Z1 = 0:1, Z2 = 0:1),
      assignment_variables = c("Z1", "Z2"),
      data = sleep,
      level = NULL
    ), NA)

  po <-
    potential_outcomes_handler(
      Y ~ Z1 + Z2,
      conditions = expand.grid(Z1 = 0:1, Z2 = 0:1),
      data = sleep
    )
  
  po2 <-
    potential_outcomes_handler(
      NULL,
      Y_Z1_0_Z2_0 = 0,
      Y_Z1_0_Z2_1 = 1,
      Y_Z1_1_Z2_0 = 1,
      Y_Z1_1_Z2_1 = 2,
      data = sleep,
      level = NULL
    )

  expect_true(all(c("Y_Z1_0_Z2_0", "Y_Z1_1_Z2_0", "Y_Z1_0_Z2_1", "Y_Z1_1_Z2_1") %in% names(po)))
  expect_true(all(c("Y_Z1_0_Z2_0", "Y_Z1_1_Z2_0", "Y_Z1_0_Z2_1", "Y_Z1_1_Z2_1") %in% names(po2)))
})


# to do: make sure xx appears in listed parameters also
test_that("environments for potential outcomes", {
  xx <- 3
  n = 2
  design <- declare_model(N = n) + 
    declare_potential_outcomes(Y ~ Z*1 + W, conditions = list(Z=c(0,1), W = c(0,xx)))
  
  rm(xx, n)

  expect_true(design |> draw_data() |> ncol() == 5)
  dots_2 <- attr(design[[2]], "dots")
  
#  env <- environment(dots_2$formula)
#  expect_true(get("xx", envir = env) == 3)

  env <- environment(dots_2$conditions)
  expect_true(get("xx", envir = env) == 3)

  find_all_objects(design)
})


# These need to be outside test environment
outcome_means = 1:3

test_that("multiarm old syntax from Design Library", {

outcome_sds = 1:3
  
N = 3
sd_i = 1

design <- declare_model(
  N = N, 
  u_1 = rnorm(N, 0, outcome_sds[1L]),
  u_2 = rnorm(N, 0, outcome_sds[2L]), 
  u_3 = rnorm(N, 0, outcome_sds[3L]),
  u = rnorm(N) * sd_i) +
  
  declare_potential_outcomes(
  formula = Y ~ 
    (outcome_means[1] + u_1) * (Z == "1") + 
    (outcome_means[2] + u_2) * (Z == "2") + 
    (outcome_means[3] + u_3) * (Z == "3") + u,
  conditions = c("1",  "2", "3"),
  assignment_variables = Z)


expect_true(ncol(draw_data(design)) == 8)

})


test_that("more old syntax", {
  
my_potential_outcomes <- declare_potential_outcomes(
  formula = Y ~ .25 * Z + .01 * age * Z,
  conditions = 1:4)

expect_true(ncol(my_potential_outcomes(data.frame(age = 1))) ==5)
})
