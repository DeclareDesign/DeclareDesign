## ---- include=FALSE, eval=TRUE-------------------------------------------
library(knitr)
library(DeclareDesign)
library(ggplot2)

## ----MIDA, echo = FALSE--------------------------------------------------
rd_template <- function(
	N = 1000,
	tau = .15,
	cutoff = .5, 
	bandwidth = cutoff, 
	poly_order = 4
){
# Model -------------------------------------------------------------------
control <- function(X) {
  as.vector(poly(X, 4, raw = T) %*% c(.7, -.8, .5, 1))}
treatment <- function(X) {
  as.vector(poly(X, 4, raw = T) %*% c(0, -1.5, .5, .8)) + tau}
population <- declare_population(
  N = N,
  X = runif(N,0,1) - cutoff,
  noise = rnorm(N,0,.1),
  Z = 1 * (X > 0))
potential_outcomes <- declare_potential_outcomes(
  Y_Z_0 = control(X) + noise,
  Y_Z_1 = treatment(X) + noise)

# Inquiry -----------------------------------------------------------------
estimand <- declare_estimand(LATE = treatment(0) - control(0))

# Data Strategy -----------------------------------------------------------
sampling <- declare_sampling(sampling_function = function(data){
  subset(data,(X > 0 - bandwidth) & X < 0 + bandwidth)})

# Answer Strategy ---------------------------------------------------------
estimator <- declare_estimator(
  formula = Y ~ poly(X, poly_order) * Z,
  model = lm,
  estimand = estimand)

# Design ------------------------------------------------------------------
declare_design(
 population, potential_outcomes, estimand, sampling, reveal_outcomes, estimator)
}
attr(rd_template,"tips") <- 
	c(N = "Size of population to sample from",
		tau = "Difference in potential outcomes functions at the threshold",
		cutoff = "Threshold on running variable beyond which units are treated", 
		bandwidth = "Bandwidth around threshold from which to include units", 
		poly_order = "Order of the polynomial regression used to estimate the jump at the cutoff"
		)
saveRDS(rd_template,"rd_template.RDS") 

## ----include=FALSE-------------------------------------------------------
control <- function(X) {
  as.vector(poly(X, 4, raw = T) %*% c(.7, -.8, .5, 1))}
treatment <- function(X) {
  as.vector(poly(X, 4, raw = T) %*% c(0, -1.5, .5, .8)) + .15}
pro_con_colors <- c("#C67800", "#205C8A")
mock_data <- draw_data(rd_template())
X <- seq(-.5,.5,.005)
treatment_frame <- data.frame(
	X = X,
	Y = treatment(X),
	observed = ifelse(X > 0,"a","b"),
	Z = 1
)
control_frame <- data.frame(
	X = X,
	Y = control(X),
	observed = ifelse(X <= 0,"a","b"),
	Z = 0
)
plot_frame <- rbind(treatment_frame, control_frame)

## ----echo=FALSE----------------------------------------------------------
ggplot(plot_frame,aes(x = X, y = Y, color = as.factor(Z))) + 
  geom_line(aes(linetype = observed)) +
  geom_point(data = mock_data, alpha = .2, size = .5) +
  scale_linetype_discrete(name = "", labels = c("Observable","Unobservable")) +
  scale_color_manual(name = "", labels = c("Untreated","Treated"),values = pro_con_colors) +
  geom_vline(xintercept = 0, size = .05) +
  xlab("Running Variable") + 
  geom_segment(aes(x = 0,xend = 0, y = control(0),yend = treatment(0)),color = "black") +
   theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = '#eeeeee'),
    strip.background = element_blank(),
    legend.position = "bottom",
    text = element_text(family = "Palatino"))

## ---- eval = FALSE-------------------------------------------------------
#  N <- 1000
#  cutoff <- .5
#  bandwidth <- cutoff
#  tau <- .15
#  poly_order <- 4
#  # Model -------------------------------------------------------------------
#  control <- function(X) {
#    as.vector(poly(X, 4, raw = T) %*% c(.7, -.8, .5, 1))}
#  treatment <- function(X) {
#    as.vector(poly(X, 4, raw = T) %*% c(0, -1.5, .5, .8)) + tau}
#  population <- declare_population(
#    N = N,
#    X = runif(N,0,1) - cutoff,
#    noise = rnorm(N,0,.1),
#    Z = 1 * (X > 0))
#  potential_outcomes <- declare_potential_outcomes(
#    Y_Z_0 = control(X) + noise,
#    Y_Z_1 = treatment(X) + noise)
#  
#  # Inquiry -----------------------------------------------------------------
#  estimand <- declare_estimand(LATE = treatment(0) - control(0))
#  
#  # Data Strategy -----------------------------------------------------------
#  sampling <- declare_sampling(sampling_function = function(data){
#    subset(data,(X > 0 - bandwidth) & 	X < 0 + bandwidth)})
#  
#  # Answer Strategy ---------------------------------------------------------
#  estimator <- declare_estimator(
#    formula = Y ~ poly(X, poly_order) * Z,
#    model = lm,
#    estimand = estimand)
#  
#  # Design ------------------------------------------------------------------
#  design <- declare_design(
#   population, potential_outcomes, estimand, sampling, reveal_outcomes, estimator)

## ----echo = FALSE, eval = FALSE------------------------------------------
#  diagnosis <- diagnose_design(rd_template(), bootstrap = TRUE, sims = 10000,bootstrap_sims = 1000)
#  saveRDS(diagnosis, file = "rd_diagnosis.RDS")

## ----echo = FALSE,eval = TRUE, include = FALSE---------------------------
diagnosis <- readRDS(file = "rd_diagnosis.RDS")

## ---- eval = FALSE, echo = TRUE------------------------------------------
#  diagnosis <- diagnose_design(rd_design, sims = 10000, bootstrap_sims = 1000)

## ----echo = FALSE--------------------------------------------------------
diagnosis_table <- get_diagnosands(diagnosis)[,c("mean_estimate","mean_estimand","bias","se(bias)","power","se(power)","coverage","se(coverage)")]
names(diagnosis_table) <- c("Mean Estimate", "Mean Estimand", "Bias", "SE(Bias)", "Power", "SE(Power)", "Coverage", "SE(Coverage)")

knitr::kable(diagnosis_table,digits = 3)

