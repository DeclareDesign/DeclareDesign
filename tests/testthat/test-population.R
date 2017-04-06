
test_that("declare_population", {

  # just N

  my_population <- declare_population(N = 100)

  my_population()

  # test multilevel

  pop <- declare_population(
    regions = level(N = 5, gdp = rnorm(N)),
    cities = level(N = sample(1:5), subways = rnorm(N, mean = gdp)))

  pop()

  pop <- declare_population(
    regions = level(N = 5),
    cities = level(N = sample(1:5), subways = rnorm(N, mean = 5)))

  pop()


  # test multilevel

  dat <- fabricatr::fabricate_data(
    districts = level(N = 25, urban = sample(0:1, N, replace = TRUE)),
    villages = level(N = 100, altitude = rnorm(N)),
    individuals = level(N = 100, income = rnorm(N),
                        age = sample(18:95, N, replace = TRUE)))


  my_population_nested <- declare_population(
    districts = level(N = 25, urban = sample(0:1, N, replace = TRUE)),
    villages = level(N = 10, altitude = rnorm(N)),
    individuals = level(N = 100, income = rnorm(N),
                        age = sample(18:95, N, replace = TRUE)))

  my_population_nested()


  # custom function

  my_population_function <- function(N) {
    data.frame(u = rnorm(N))
  }

  my_population_custom <- declare_population(
    population_function = my_population_function, N = 100)

  ## works
  rm(my_population_function)

  ##debugonce(DeclareDesign:::lazy_eval_DD)

  rm(list = ls()[!(ls() %in% c("my_population_custom"))])

  ##debugonce(my_population_custom)
  pop_custom <- my_population_custom()

  head(pop_custom)

  ## default function

  my_population_default <- declare_population(N = 100, q = rnorm(5))

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

  declare_population(districts = level(N = 5, gdp = rnorm(N)),
                     villages = level(N = 12, subways = rnorm(N, mean = gdp)))()

})
