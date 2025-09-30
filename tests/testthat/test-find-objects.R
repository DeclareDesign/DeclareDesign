context("Find objects")

test_that("Steps and designs", {

  x <- 2
  y  <- 3
  k <- 6
  
  step_1 <- declare_model(N = x)
  step_2 <- declare_model(X = 2*x, Y = y)

  design <- step_1 + step_2
  
  expect_true(find_all_objects(step_1) |> nrow() ==1)
  expect_true(find_all_objects(step_2) |> nrow()  ==2)
  expect_true(find_all_objects(design)|> nrow() ==3)

})


test_that("Right objects", {
  x = 10
  b = 2
  design <-
    declare_model(N = x, Y = runif(N)) +
    declare_inquiry(ATE = b) +
    declare_estimator(Y ~1)
  
  ob <- find_all_objects(design)
  expect_true(all(ob$name == c("x", "b")))
  expect_true(all(ob$step == 1:2))
  

})


# there is a minor issue that an object might get saved even if it is irrelevant if it has
# the same name as an object that will later appear in data
# this appears to be harmless
test_that("promises", {

  x1 = 1  
  x2 = 75
  step_1 <- declare_model(N = x1, x2 = 1)
  step_2 <- declare_model(Y = x2)
  
  design <- step_1 + step_2
  expect_true(draw_data(design)$Y ==1)
  
  design <- redesign(design, x2 = 133)
  expect_true(draw_data(design)$Y ==1)
  
})


test_that("promises", {
 x2 = 1  
 # if not there, it cannot be added through redesign  
 design <- declare_model(N = x1, x2 = x2) + NULL
 # design formed even though objects missing
 expect_true(all(find_all_objects(design) |> dim() == c(1,5)))
 expect_error(draw_data(design))
 design <- redesign(design, x2 = 1) 
 expect_warning(design <- redesign(design, x1 = 1), 
                "You requested a change to x1 but x1 is not found in the design") 
})

test_that("No mix ups", {
  
  sd <- 2

  step_1 <- declare_model(N = 1, sd = sd^2)
  step_2 <- declare_model(sd2 = sd)
  
  design <- step_1 + step_2
  
  design
  
  # sd2 should be 4 not 2 since data should be prioritized
  expect_true(draw_data(design)$sd2 == 4)
  
  # future: ideally there should NOT be a quosure for sd2 (containing sd)
  # expect_true(!any(find_all_objects(design)$quosure == "sd2"))

  
})

