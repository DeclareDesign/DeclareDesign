context("Population")

test_that("declare_population", {

  # just N

  my_population <- declare_population(N = 10)
  my_population()

  # test multilevel

  pop <- declare_population(
    regions = add_level(N = 2, gdp = rnorm(N)),
    cities = add_level(N = sample(1:2), subways = rnorm(N, mean = gdp)))

  pop()

  pop <- declare_population(
    regions = add_level(N = 5),
    cities = add_level(N = sample(1:5), subways = rnorm(N, mean = 5)))

  pop()


  # test multilevel

  pop <- declare_population(
    districts = add_level(N = 25, urban = sample(0:1, N, replace = TRUE)),
    villages = add_level(N = 10, altitude = rnorm(N)),
    individuals = add_level(N = 10, income = rnorm(N),
                        age = sample(18:95, N, replace = TRUE)))

  head(pop())

})

test_that("custom declare", {

  # custom function

  my_population_function <- function(N) {
    data.frame(u = rnorm(N))
  }

  my_population_custom <- declare_population(
    handler = my_population_function, N = 100)

  ## works
  rm(my_population_function)

  ##debugonce(DeclareDesign:::lazy_eval_DD)

  rm(list = ls()[!(ls() %in% c("my_population_custom"))])

  ##debugonce(my_population_custom)
  pop_custom <- my_population_custom()

  head(pop_custom)

  ## default function

  my_population_default <- declare_population(N = 100, q = rnorm(100))

  ## works
  pop_default <- my_population_default()

  head(pop_default)

  ## do quick design

  design_func <- function(numb){
    pop <- declare_population(N = numb, q = rnorm(5))
    rm(numb)
    return(pop)
  }

  rm(list = ls()[!(ls() %in% "design_func")])
  design_func(numb = 5)()

  my_design <- design_func(numb = 5)

  declare_population(N = 10, Y1 = rnorm(N), Y2 = rnorm(N))()

  declare_population(districts = add_level(N = 5, gdp = rnorm(N)),
                     villages = add_level(N = 12, subways = rnorm(N, mean = gdp)))()

})


test_that("use custom data with declare_population", {
  region_data <- data.frame(regions = as.character(26:30), capital = c(1, 0, 0, 0, 0), stringsAsFactors = FALSE)

  ## create single-level data taking user data
  declare_population(data = region_data)() %>% head

  ## create single-level data taking user data and adding variables
  declare_population(data = region_data, gdp = rnorm(N))() %>% head

  ## create multi-level data with data only at the top level, adding variables at each level
  #debugonce(level)
  declare_population(
    regions = add_level(N = 2,
                    gdp = rnorm(N), ID_label = regions),
    villages = add_level(N = 12, subways = rnorm(N, mean = gdp)))() %>% head

  ## create multi-level data with data at each level, merging on the ID variables of upper levels
  villages_data <- data.frame(regions = as.character(rep(1:5, each = 5)),
                              altitude = rnorm(5*5), stringsAsFactors = FALSE)

  declare_population(
    regions = add_level(N = 2, gdp = rnorm(N),
                    ID_label = regions),
    villages = add_level(N = 2,
                     by = regions, subways = rnorm(N, mean = gdp)))() %>% head

  ## also should work with a single level
  declare_population(
    regions = add_level(N = 2, gdp = rnorm(N),
                    ID_label = regions))() %>% head

  # and you can mix level( with other things
  declare_population(
    regions = add_level(N = 2, gdp = rnorm(N),
                    ID_label = regions),
    villages = add_level(N = 3,
                     by = regions, subways = rnorm(N, mean = gdp)))() %>% head

})


test_that("test data generation functions", {

  # what are these
  my_function <- function(x) x + 10
  a <- 500

  # Simple 1-level cases

  one_lev1 <- declare_population(
    N = 10,
    income = rnorm(N),
    age = rpois(10, 30)
  )
  one_lev2 <- declare_population(
    level_A = add_level(N = 10, income = rnorm(N),
                    age = rpois(n = N, 30 ))
  )

  one_lev1()
  one_lev2()

  # Simple multi-level cases
  multi_lev1 <- declare_population(
    region = add_level(N = 2),
    city = add_level(
      N = 5,
      city_educ_mean = rnorm(n = N, mean = 100, sd = 10),
      city_educ_sd = rgamma(n = N, shape = 2, rate = 2)
    ),
    indiv = add_level(
      N = 10,
      income = rnorm(N),
      age = rpois(N,30)
    )
  )

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
      age = rpois(N,30),
      income_times_age = income*age
    )
  )

  multi_lev1()
  multi_lev2()

})



test_that("Population declaration variations", {

  my_population <- declare_population(N = 10, noise = rnorm(N))
  my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))

  # Way 1

  design <- declare_design(my_population, my_potential_outcomes)
  draw_data(design)

  # Way 2

  design <- declare_design(my_population(), my_potential_outcomes)
  draw_data(design)

  # Way 3

  df <- my_population()
  design <- declare_design(df, my_potential_outcomes)
  draw_data(design)


})



