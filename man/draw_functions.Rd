% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_functions.R
\name{draw_functions}
\alias{draw_functions}
\alias{draw_data}
\alias{draw_estimand}
\alias{draw_estimands}
\alias{draw_estimates}
\title{Draw data, estimates, and inquiries from a design}
\usage{
draw_data(design, data = NULL, start = 1, end = length(design))

draw_estimand(...)

draw_estimands(...)

draw_estimates(...)
}
\arguments{
\item{design}{A design object, typically created using the + operator}

\item{data}{A data.frame object with sufficient information to get the data, estimates, inquiries, an assignment vector, or a sample.}

\item{start}{(Defaults to 1) a scalar indicating which step in the design to begin with. By default all data steps are drawn, from step 1 to the last step of the design.}

\item{end}{(Defaults to \code{length(design)}) a scalar indicating which step in the design to finish drawing data by.}

\item{...}{A design or set of designs typically created using the + operator}
}
\description{
Draw data, estimates, and inquiries from a design
}
\examples{

# Two-arm randomized experiment
design <-
  declare_model(
    N = 500,
    gender = rbinom(N, 1, 0.5),
    X = rep(c(0, 1), each = N / 2),
    U = rnorm(N, sd = 0.25),
    potential_outcomes(Y ~ 0.2 * Z + X + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(S = complete_rs(N = N, n = 200)) +
  declare_assignment(Z = complete_ra(N = N, m = 100)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")

# Use draw_data to create a dataset using a design
dat <- draw_data(design)

# Use end argument to draw data up to a certain design component
dat_no_sampling <- draw_data(design, end = 3)

# Use draw_estimands to extract value of inquiry
draw_estimands(design)

# Use draw_estimates to extract value of estimator
draw_estimates(design)

}
