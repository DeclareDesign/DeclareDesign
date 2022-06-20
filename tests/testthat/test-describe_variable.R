context("Describe Variable")

test_that("Describle Variable", {
  data_littleint <- c(rep(1, 10), rep(2, 10), rep(3, 10))
  data_littlechar <- c(rep("test", 10), rep("that", 10))
  data_littlefactor <- as.factor(data_littlechar)
  data_littlemissing <- c(rep(1, 10), rep(NA, 3))

  data_bigint <- 1:20
  data_bigchar <- c("a", "b", "c", "d", "e", "f", "g")
  data_bigfactor <- as.factor(data_bigchar)
  data_bigmissing <- c(1:10, rep(NA, 3))

  data_logical <- c(rep(TRUE, 11), rep(FALSE, 9))
  data_true_mixed <- c(TRUE, 5, NA, "test") # Should induce coercion to character
  data_mixed_numeric <- c(TRUE, FALSE, TRUE, 5, 7, 8, 9) # Should induce coercion to 1/0

  describe_variable(data_littleint)
  describe_variable(data_littlechar)
  describe_variable(data_littlefactor)
  describe_variable(data_littlemissing)

  describe_variable(data_bigint)
  describe_variable(data_bigchar)
  describe_variable(data_bigfactor)
  describe_variable(data_bigmissing)

  describe_variable(data_logical)

  expect_equal(
    sort(colnames(describe_variable(data_true_mixed))),
    sort(c("5", "test", "TRUE", "NA"))
  )

  expect_equal(
    describe_variable(data_mixed_numeric),
    structure(list(
      min = 0, median = 5, mean = 4.43, max = 9, sd = 3.74,
      N_missing = 0L, N_unique = 6L
    ),
    .Names = c(
      "min", "median",
      "mean", "max", "sd", "N_missing", "N_unique"
    ),
    row.names = c(NA, -1L), class = "data.frame"
    )
  )
})


test_that("Describe Variable", {
  test <- data.frame(
    d = seq(from = as.POSIXct("2018-01-01"),
            to = as.POSIXct("2018-05-01"),
            by = "days"))
  # Different ways of bringing data into DD
  pop1 <- declare_model(data = test)
  dsgn <- pop1 + NULL 
  expect_output(print(dsgn, verbose = TRUE), "2018-01-01")
})
