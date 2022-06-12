context("post estimation")

test_that("multiple design draw_estimates", {
  my_designer <- function(N = 50) {
    my_population <- declare_model(N = N, noise = rnorm(N))
    
    my_potential_outcomes <-
      declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
    
    my_assignment <- declare_assignment(Z = complete_ra(N, m = 25))
    
    my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
    
    my_estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry)
    
    my_measurement <- declare_measurement(Y = reveal_outcomes(Y ~ Z)) 
    
    my_design <-
      my_population +
      my_potential_outcomes +
      my_inquiry +
      my_assignment +
      my_measurement +
      my_estimator
    
    my_design
  }
  
  my_population <- declare_model(N = 100, noise = rnorm(N))
  
  my_potential_outcomes <-
    declare_model(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2))
  
  my_assignment <- declare_assignment(Z = complete_ra(N, m = 25))
  
  my_inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
  
  my_estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry)
  
  my_measurement <- declare_measurement(Y = reveal_outcomes(Y ~ Z)) 
  
  design_1 <- my_population +
    my_potential_outcomes +
    my_inquiry +
    my_assignment +
    my_measurement +
    my_estimator
  
  my_assignment_2 <- declare_assignment(Z = complete_ra(N, m = 50))
  
  design_2 <- replace_step(design_1, my_assignment, my_assignment_2)
  
  my_designs <- expand_design(my_designer, N = c(50, 100))
  
  draw_estimands(design_1)
  
  draw_estimands(design_2)
  
  draw_estimands(design_1, design_2)
  
  draw_estimands(my_designs)
  
  draw_estimates(design_1)
  
  draw_estimates(design_2)
  
  expect_equal(draw_estimates(design_1, design_2)$design,
               c("design_1", "design_2"))
  
  draw_estimates(my_designs)
})

test_that("glance works", {
  des <-
    declare_model(data = sleep) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = ~ glance(.),
      label = "formula call"
    ) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = glance,
      label = "bare function"
    ) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = "glance",
      label = "string"
    )
  
  est <- draw_estimates(des)
  
  expect_equal(est,
               structure(
                 list(
                   estimator = c("formula call", "bare function",
                                       "string"),
                   r.squared = c(0.161332850791025, 0.161332850791025,
                                 0.161332850791025),
                   adj.r.squared = c(0.114740231390526, 0.114740231390526,
                                     0.114740231390526),
                   statistic = c(3.46262676078045, 3.46262676078045,
                                 3.46262676078045),
                   p.value = c(0.079186714215938, 0.079186714215938,
                               0.079186714215938),
                   df.residual = c(18, 18, 18),
                   nobs = c(20L, 20L,
                         20L),
                   se_type = c("HC2", "HC2", "HC2")
                 ),
                 row.names = c(NA,-3L),
                 class = "data.frame"
               ))
  
})

test_that("tidy works", {
  # default term
  des <-
    declare_model(data = sleep) +
    declare_estimator(extra ~ group, .method = lm_robust, label = "formula")
  est <- draw_estimates(des)
  expect_equal(est, structure(
    list(
      estimator = "formula",
      term = "group2",
      estimate = 1.58,
      std.error = 0.849091017238762,
      statistic = 1.86081346748685,
      p.value = 0.0791867142159381,
      conf.low = -0.203874032287598,
      conf.high = 3.3638740322876,
      df = 18,
      outcome = "extra"
    ),
    row.names = c(NA, -1L),
    class = "data.frame"
  ))
  
  # default term
  des <-
    declare_model(data = sleep) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = ~ tidy_try(.),
      label = "formula"
    ) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = tidy_try,
      label = "bare"
    ) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = "tidy_try",
      label = "string"
    )
  
  est <- draw_estimates(des)
  expect_equal(est, structure(
    list(
      estimator = c("formula", "bare", "string"),
      term = c("group2", "group2", "group2"),
      estimate = c(1.58,
                   1.58, 1.58),
      std.error = c(0.849091017238762, 0.849091017238762,
                    0.849091017238762),
      statistic = c(1.86081346748685, 1.86081346748685,
                    1.86081346748685),
      p.value = c(0.0791867142159381, 0.0791867142159381,
                  0.0791867142159381),
      conf.low = c(-0.203874032287598, -0.203874032287598, -0.203874032287598),
      conf.high = c(3.3638740322876, 3.3638740322876,
                    3.3638740322876),
      df = c(18, 18, 18),
      outcome = c("extra", "extra",
                  "extra")
    ),
    row.names = c(NA, -3L),
    class = "data.frame"
  ))
  
  # another default
  des <-
    declare_model(data = sleep) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = ~ tidy_try(.),
      term = FALSE,
      label = "formula1"
    ) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = tidy_try,
      term = FALSE,
      label = "bare"
    ) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = "tidy_try",
      term = FALSE,
      label = "string"
    )
  
  est <- draw_estimates(des)
  expect_equal(est, structure(
    list(
      estimator = c("formula1", "bare", "string"),
      term = c("group2", "group2", "group2"),
      estimate = c(1.58,
                   1.58, 1.58),
      std.error = c(0.849091017238762, 0.849091017238762,
                    0.849091017238762),
      statistic = c(1.86081346748685, 1.86081346748685,
                    1.86081346748685),
      p.value = c(0.0791867142159381, 0.0791867142159381,
                  0.0791867142159381),
      conf.low = c(-0.203874032287598,-0.203874032287598,-0.203874032287598),
      conf.high = c(3.3638740322876, 3.3638740322876,
                    3.3638740322876),
      df = c(18, 18, 18),
      outcome = c("extra", "extra",
                  "extra")
    ),
    row.names = c(NA,-3L),
    class = "data.frame"
  ))
  
  # return all coefs
  des <-
    declare_model(data = sleep) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = ~ tidy_try(.),
      term = TRUE,
      label = "formula2"
    ) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = tidy_try,
      term = TRUE,
      label = "bare"
    ) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = "tidy_try",
      term = TRUE,
      label = "string"
    )
  
  est <- draw_estimates(des)
  expect_equal(est, structure(
    list(
      estimator = c("formula2", "formula2", "bare",
                          "bare", "string", "string"),
      term = c(
        "(Intercept)",
        "group2",
        "(Intercept)",
        "group2",
        "(Intercept)",
        "group2"
      ),
      estimate = c(0.75,
                   1.58, 0.75, 1.58, 0.75, 1.58),
      std.error = c(
        0.565734527455728,
        0.849091017238762,
        0.565734527455728,
        0.849091017238762,
        0.565734527455728,
        0.849091017238762
      ),
      statistic = c(
        1.32571014071382,
        1.86081346748685,
        1.32571014071382,
        1.86081346748685,
        1.32571014071382,
        1.86081346748685
      ),
      p.value = c(
        0.201515544020674,
        0.0791867142159381,
        0.201515544020674,
        0.0791867142159381,
        0.201515544020674,
        0.0791867142159381
      ),
      conf.low = c(
        -0.438564137657087,-0.203874032287598,
        -0.438564137657087,
        -0.203874032287598,
        -0.438564137657087,-0.203874032287598
      ),
      conf.high = c(
        1.93856413765709,
        3.3638740322876,
        1.93856413765709,
        3.3638740322876,
        1.93856413765709,
        3.3638740322876
      ),
      df = c(18, 18, 18, 18, 18, 18),
      outcome = c("extra", "extra",
                  "extra", "extra", "extra", "extra")
    ),
    row.names = c(NA,-6L),
    class = "data.frame"
  ))
  
  # select them manually
  des <-
    declare_model(data = sleep) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = ~ tidy_try(.),
      term = "group2",
      label = "formula2"
    ) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = tidy_try,
      term = "group2",
      label = "bare"
    ) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = "tidy_try",
      term = "group2",
      label = "string"
    )
  
  est <- draw_estimates(des)
  expect_equal(est, structure(
    list(
      estimator = c("formula2", "bare", "string"),
      term = c("group2", "group2", "group2"),
      estimate = c(1.58,
                   1.58, 1.58),
      std.error = c(0.849091017238762, 0.849091017238762,
                    0.849091017238762),
      statistic = c(1.86081346748685, 1.86081346748685,
                    1.86081346748685),
      p.value = c(0.0791867142159381, 0.0791867142159381,
                  0.0791867142159381),
      conf.low = c(-0.203874032287598,-0.203874032287598,-0.203874032287598),
      conf.high = c(3.3638740322876, 3.3638740322876,
                    3.3638740322876),
      df = c(18, 18, 18),
      outcome = c("extra", "extra",
                  "extra")
    ),
    row.names = c(NA,-3L),
    class = "data.frame"
  ))
  
  
  # select them manually
  des <-
    declare_model(data = sleep) +
    # does not work (intentionally)
    # declare_estimator(
    #   extra ~ group,
    #   .method = lm_robust,
    #   .summary = ~ tidy_try(., term = group2),
    #   label = "formula1"
    # ) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = ~ tidy_try(.),
      term = group2,
      label = "formula2"
    ) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = tidy_try,
      term = group2,
      label = "bare"
    ) +
    declare_estimator(
      extra ~ group,
      .method = lm_robust,
      .summary = "tidy_try",
      term = group2,
      label = "string"
    )
  
  est <- draw_estimates(des)
  
  expect_equal(est, structure(
    list(
      estimator = c("formula2", "bare", "string"),
      term = c("group2", "group2", "group2"),
      estimate = c(1.58,
                   1.58, 1.58),
      std.error = c(0.849091017238762, 0.849091017238762,
                    0.849091017238762),
      statistic = c(1.86081346748685, 1.86081346748685,
                    1.86081346748685),
      p.value = c(0.0791867142159381, 0.0791867142159381,
                  0.0791867142159381),
      conf.low = c(-0.203874032287598,-0.203874032287598,-0.203874032287598),
      conf.high = c(3.3638740322876, 3.3638740322876,
                    3.3638740322876),
      df = c(18, 18, 18),
      outcome = c("extra", "extra",
                  "extra")
    ),
    row.names = c(NA,-3L),
    class = "data.frame"
  ))
  
})
