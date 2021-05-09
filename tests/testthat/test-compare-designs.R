context("Compare Designs")

my_population <- declare_population(N = 50, noise = rnorm(N))

my_potential_outcomes <-
  declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

my_assignment <- declare_assignment(Z = complete_ra(N, m = 25))

pate <- declare_inquiry(pate = mean(Y_Z_1 - Y_Z_0))
sate <- declare_inquiry(sate = mean(Y_Z_1 - Y_Z_0))

pate_estimator <- declare_estimator(Y ~ Z, inquiry = pate)
sate_estimator <- declare_estimator(Y ~ Z, inquiry = sate)

reveal <- declare_reveal()

my_design_1 <- my_population +
  my_potential_outcomes +
  pate +
  my_assignment +
  reveal +
  pate_estimator

my_design_2 <- my_population +
  my_potential_outcomes +
  sate +
  my_assignment +
  reveal +
  sate_estimator

test_that("compare_designs works", {
  
  
  diagnosis_1 <- diagnose_design(my_design_1, sims = 2, bootstrap_sims = FALSE)
  diagnosis_2 <- diagnose_design(my_design_2, sims = 2, bootstrap_sims = FALSE)
  
  # designs not in list, no names, names are imputed
  comparison <- diagnose_design(my_design_1, my_design_2, sims = 2, bootstrap_sims = FALSE)
  expect_equal(as.character(comparison$diagnosands$design), c("my_design_1", "my_design_2"))
  
  # designs in list, no names, names are imputed
  comparison <- diagnose_design(list(my_design_1, my_design_2), sims = 2, bootstrap_sims = FALSE)
  expect_equal(as.character(comparison$diagnosands$design), c("design_1", "design_2"))
  
  # designs not in list, all names, names used
  comparison <- diagnose_design(d1 = my_design_1, d2 = my_design_2, sims = 2, bootstrap_sims = FALSE)
  expect_equal(as.character(comparison$diagnosands$design), c("d1", "d2"))
  
  # designs in list, all names, names used
  comparison <- diagnose_design(list(d1 = my_design_1, d2 = my_design_2), sims = 2, bootstrap_sims = FALSE)
  expect_equal(as.character(comparison$diagnosands$design), c("d1", "d2"))
  
  # designs not in list, some names, available names used
  comparison <- diagnose_design(my_design_1, a_design_2 = my_design_2, sims = 2, bootstrap_sims = FALSE)
  expect_true(all(as.character(comparison$diagnosands$design) %in% c("my_design_1", "a_design_2")))
  
  # designs not in list, duplicated names used, error
  expect_error(comparison <- diagnose_design(d1 = my_design_1, d1 = my_design_2, sims = 2, bootstrap_sims = FALSE))
  
  # designs in list, duplicated names used, error
  expect_error(comparison <- diagnose_design(list(d1 = my_design_1, d1 = my_design_2), sims = 2, bootstrap_sims = FALSE))
})


my_population <- declare_population(N = 50, noise = rnorm(N))

my_potential_outcomes <-
  declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

my_assignment <- declare_assignment(Z = complete_ra(N, m = 25))

pate <- declare_inquiry(pate = mean(Y_Z_1 - Y_Z_0))
sate <- declare_inquiry(sate = mean(Y_Z_1 - Y_Z_0))

pate_estimator <- declare_estimator(Y ~ Z, inquiry = pate)
sate_estimator <- declare_estimator(Y ~ Z, inquiry = sate)

reveal <- declare_reveal()

my_special_step <- declare_inquiry(ATE = 5)

my_design_3 <- my_population +
  my_potential_outcomes +
  pate +
  my_special_step +
  my_assignment +
  reveal +
  pate_estimator

my_design_4 <- my_population +
  my_potential_outcomes +
  sate +
  my_assignment +
  reveal +
  sate_estimator

test_that("compare works", {
  a <- compare_design_code(my_design_3, my_design_4)
  b <- compare_design_summaries(my_design_3, my_design_4)
  c <- compare_design_data(my_design_3, my_design_4)
  d <- compare_design_inquiries(my_design_3, my_design_4)
  e <- compare_design_estimates(my_design_3, my_design_4)
  f <- compare_designs(my_design_3, my_design_4)
})


