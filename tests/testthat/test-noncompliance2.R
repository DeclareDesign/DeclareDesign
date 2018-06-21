context("noncompliance2")

test_that("POs correctly assembled for noncompliance case", {

pop <- declare_population(N = 10000,
                          type = sample(
                            c("Complier", "Never-taker", "Always-taker"),
                            size = N,
                            prob = c(0.5, 0.2, 0.3),
                            replace = TRUE),
                          noise = rnorm(N))

df <- pop()

pos_D <- declare_potential_outcomes(D ~ as.numeric(type == "Always-taker" | type == "Complier" & Z == 1))


expect_equal(colnames(pos_D(df)), c("ID", "type", "noise", "D_Z_0", "D_Z_1"))

pos_Y <- declare_potential_outcomes(
  Y ~ 0.4 * D * (type == "Complier") -0.2 * (type == "Never-taker") +
    0.5 * (type == "Always-taker") +
    noise, assignment_variables = "D"
)

assignment <- declare_assignment(prob = 0.5)

suppressWarnings(
  noncompliance <-
    pop + 
    pos_D + 
    assignment + 
    #declare_reveal(D, Z) + 
    pos_Y + 
    declare_reveal(Y, D)
)

e <- (noncompliance[[4]])

expect_true(inherits(e, "design_step"))
expect_equal(attr(e, "step_type"), "reveal")
expect_equal(attr(e, "step_meta")$assignment_variables, "Z")
expect_equal(attr(e, "step_meta")$outcome_variables, "D")

})


test_that("POS don't erase Z",{

  pop <- declare_population(N = 10, Z = rbinom(N, size = 1, prob = .5))
  po <- declare_potential_outcomes(Y ~ Z)
  df <- pop()
  expect_equal(df$Z, po(df)$Z)
})
