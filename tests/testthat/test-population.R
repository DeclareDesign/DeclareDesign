context("Population")

test_that("declare_population N=10", {
  my_population <- declare_population(N = 10)
  expect_equal(nrow(my_population()), 10)
})

test_that("declare_population multilevel N=2", {
  pop <- declare_population(
    regions = add_level(N = 2, gdp = rnorm(N)),
    cities = add_level(N = sample(1:2), subways = rnorm(N, mean = gdp))
  )

  expect_equal(pop() %>% colnames(), c("regions", "gdp", "cities", "subways"))
})

test_that("declare_population multilevel N=5", {
  pop <- declare_population(
    regions = add_level(N = 5),
    cities = add_level(N = sample(1:5), subways = rnorm(N, mean = 5))
  )

  expect_equal(pop() %>% with(unique(regions)), as.character(1:5))
})

test_that("declare_population multilevel 3 levels", {
  pop <- declare_population(
    districts = add_level(N = 25, urban = sample(0:1, N, replace = TRUE)),
    villages = add_level(N = 10, altitude = rnorm(N)),
    individuals = add_level(
      N = 10, income = rnorm(N),
      age = sample(18:95, N, replace = TRUE)
    )
  )

  expect_length(pop() %>% with(unique(individuals)), 25 * 10 * 10)
})

test_that("custom declare", {

  # custom function

  my_population_function <- function(N) {
    data.frame(u = rnorm(N))
  }

  my_population_custom <- declare_population(
    handler = my_population_function, N = 100
  )

  rm(list = ls()[!(ls() %in% c("my_population_custom"))])

  pop_custom <- my_population_custom()

  expect_equal(nrow(pop_custom), 100)
})

test_that("default function", {
  my_population_default <- declare_population(N = 100, q = rnorm(100))

  ## works
  pop_default <- my_population_default()

  expect_equal(nrow(pop_default), 100)
})

test_that("inside function", {

  ## do quick design

  design_func <- function(numb) {
    pop <- declare_population(N = numb, q = rnorm(5))
    rm(numb)
    return(pop)
  }

  rm(list = ls()[!(ls() %in% "design_func")])

  my_design <- design_func(numb = 5)

  expect_equal(nrow(my_design()), 5)
})


test_that("Two level", {
  d <- declare_population(
    districts = add_level(N = 5, gdp = rnorm(N)),
    villages = add_level(N = 12, subways = rnorm(N, mean = gdp))
  )()

  expect_length(unique(d$villages), 5 * 12)
})


test_that("use custom data with declare_population", {
  region_data <- data.frame(regions = as.character(26:30), capital = c(1, 0, 0, 0, 0), stringsAsFactors = FALSE)

  ## create single-level data taking user data
  d1 <- declare_population(data = region_data)()

  expect_identical(region_data, d1)

  ## create single-level data taking user data and adding variables
  d2 <- declare_population(data = region_data, gdp = 1:N)()


  expect_equal(d2$gdp, 1:5)
  d2$gdp <- NULL

  expect_true(!anyDuplicated(d2$ID))
  d2$ID <- NULL


  expect_identical(region_data, d2)
})


test_that("test data generation functions - single level add level", {

  # Simple 1-level cases
  one_lev1 <- declare_population(
    N = 10,
    income = 1:N,
    age = seq(10, 30, length.out = N),
    ID_label = "level_A"
  )
  one_lev2 <- declare_population(
    level_A = add_level(
      N = 10,
      income = 1:N,
      age = seq(10, 30, length.out = N)
    )
  )

  expect_identical(one_lev1(), one_lev2())
})


test_that("test multi level with transformations", {

  # With transformations within levels
  multi_lev2 <- declare_population(
    region = add_level(N = 2),
    city = add_level(
      N = 5,
      city_educ_mean = rnorm(n = N, mean = 100, sd = 10),
      city_educ_sd = rgamma(n = N, shape = 2, rate = 2),
      city_educ_zscore = scale(city_educ_mean)
    ),
    indiv = add_level(
      N = 10,
      income = rnorm(N),
      age = rpois(N, 30),
      income_times_age = income * age
    )
  )


  out <- multi_lev2()

  expect_identical(out$income_times_age, out$income * out$age)
})



test_that("Population declaration variations", {
  my_population <- declare_population(N = 10, noise = 1:N)
  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + 2)

  # Way 1

  design <- my_population + my_potential_outcomes
  df1 <- draw_data(design)

  # Way 2

  fixed_df <- my_population()
  design <- declare_population(fixed_df) + my_potential_outcomes
  df2 <- draw_data(design)

  # Way 3

  design <- declare_population(my_population()) + my_potential_outcomes
  df3 <- draw_data(design)

  expect_identical(df1, df2)
  expect_identical(df1, df3)
})
