rm(list=ls())

library(dd)

default_diagnosands <- declare_diagnosands(
  bias = mean(est - estimand),
  power = mean(p < 0.05),
  rmse = sqrt(mean((est - estimand)^2)),
  power = mean(p < .05),
  coverage = mean(estimand <= ci_upper & estimand >= ci_lower),
  mean_estimate = mean(est),
  sd_estimate = sd(est),
  type_s_rate = mean((sign(est) != sign(estimand)) & p < .05),
  mean_estimand = mean(estimand))

save(default_diagnosands, file = "data/diagnosands.RData", compress = TRUE)
