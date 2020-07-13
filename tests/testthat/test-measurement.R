context("Measurement")

# This test is lightweight because it's just fabricate

test_that("declare_measurement works", {
  design <-
    declare_population(N = 10, latent = seq(0, 1, length.out = N)) +
    declare_measurement(observed = as.numeric(cut(latent, breaks = seq(0, 1, length.out = 6), include.lowest = TRUE)))
  

  
  
  A <- 
  structure(
    list(
      ID = c("01", "02", "03", "04", "05", "06", "07",
             "08", "09", "10"),
      latent = c(
        0,
        0.111111111111111,
        0.222222222222222,
        0.333333333333333,
        0.444444444444444,
        0.555555555555556,
        0.666666666666667,
        0.777777777777778,
        0.888888888888889,
        1
      ),
      observed = c(1, 1,
                   2, 2, 3, 3, 4, 4, 5, 5)
    ),
    class = "data.frame",
    row.names = c(NA,
                  10L)
  )
  
  
  expect_equal(A, draw_data(design))
  
  
})



