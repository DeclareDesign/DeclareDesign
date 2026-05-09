simple_design <- function(N = 50, ate = 0.3) {
  declare_model(N = N, U = rnorm(N), Y_Z_1 = U + ate, Y_Z_0 = U) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = sample(rep(0:1, length.out = N))) +
    declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
    declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                      label = "ols")
}
