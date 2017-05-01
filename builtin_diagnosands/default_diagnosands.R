rm(list = ls())

library(DeclareDesign)

default_diagnosands <- declare_diagnosands(
  bias = mean(est - estimand),
  rmse = sqrt(mean((est - estimand)^2)),
  power = mean(p < .05),
  coverage = mean(estimand <= ci_upper & estimand >= ci_lower),
  mean_estimate = mean(est),
  sd_estimate = sd(est),
  type_s_rate = mean((sign(est) != sign(estimand)) & p < .05),
  mean_estimand = mean(estimand))

save(default_diagnosands, file = "R/sysdata.rda")

