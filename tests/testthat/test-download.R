context("download")
test_that("download design", {

 ##two_arm_design <- download_design(two_arm_design)

  my_population <- declare_population(N = 100, noise = rnorm(N))
  my_potential_outcomes <- declare_potential_outcomes(
    Y_Z_0 = noise, Y_Z_1 = noise + .25)
  my_assignment <- declare_assignment(m = 100/2)
  ate <- declare_estimand(ate = mean(Y_Z_1 - Y_Z_0))
  ate_estimator <- declare_estimator(Y ~ Z, estimand = ate, label = "ate_hat")
  two_arm_design <- declare_design(my_population,
                                   my_potential_outcomes,
                                   ate,
                                   my_assignment,
                                   reveal_outcomes,
                                   ate_estimator)

 diagnose_design(two_arm_design, sims = 2, bootstrap = FALSE, parallel = FALSE)
})

test_that("download template", {

 ##two_arm_template <- download_template(two_arm_template)

  two_arm_template <- function(N){

    my_population <- declare_population(N = N, noise = rnorm(N))
    my_potential_outcomes <- declare_potential_outcomes(
      Y_Z_0 = noise, Y_Z_1 = noise + .25)
    my_assignment <- declare_assignment(m = N/2)
    ate <- declare_estimand(ate = mean(Y_Z_1 - Y_Z_0))
    ate_estimator <- declare_estimator(Y ~ Z, estimand = ate, label = "ate_hat")
    my_design <- declare_design(my_population,
                                my_potential_outcomes,
                                ate,
                                my_assignment,
                                reveal_outcomes,
                                ate_estimator)
    return(my_design)
  }


 two_arm_design <- quick_design(N = 10, template = two_arm_template)

 diagnose_design(two_arm_design, sims = 2, bootstrap = FALSE, parallel = FALSE)

})
