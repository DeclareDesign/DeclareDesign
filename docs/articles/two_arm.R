## ----MIDA, echo = FALSE,include = FALSE----------------------------------
library(DeclareDesign)

two_arm_template <- function(
  N = c(500, 100, 1000, 2000),
  n = c(250, 50, 500, 1000),
  m = c(100, 10, 30, 50, 75, 150, 200), 
  tau = c(1, .1, .5, 1.5, 2), 
  sigma = c(1, .1, .5, 1.5, 2) ) 
{
  N <- as.numeric(N[1])
  n <- as.numeric(n[1])
  m <- as.numeric(m[1])
  tau <- as.numeric(tau[1])
  sigma<- as.numeric(sigma[1])
  if(n > N) stop("n > N"); as.numeric(0)
  if(m > n) stop("m > n"); as.numeric(0)
    
  population <- declare_population( 
   N = N, noise = rnorm(N), treatment_effect = rnorm(N, mean = tau, sd = sigma))
  potential_outcomes <- declare_potential_outcomes(
  	Y_Z_0 = noise, 
  	Y_Z_1 = noise + treatment_effect)
	sampling <- declare_sampling(n = n)
	assignment <- declare_assignment(m = m)
	estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
	estimator <- declare_estimator(Y ~ Z, estimand = estimand)
	declare_design(
		population, potential_outcomes, sampling, estimand, assignment, reveal_outcomes, estimator)
}

attr(two_arm_template,"tips") <- c(
	N = "Size of population", n = "Size of sample", 
	m = "Number assigned to treatment", tau = "Average treatment effect", 
	sigma = "Additional (square root of) variance added by treatment"
)



## ------------------------------------------------------------------------
# Model ------------------------------------------------------------------------
N <- 500
n <- 250
m <- 100
tau <- 1
sigma <- 3
population <- declare_population( 
 N = N, noise = rnorm(N), treatment_effect = rnorm(N, mean = tau, sd = sigma))
potential_outcomes <- declare_potential_outcomes(
	Y_Z_0 = noise, 
	Y_Z_1 = noise + treatment_effect)

# Inquiry ----------------------------------------------------------------------
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

# Data Strategy ----------------------------------------------------------------
sampling <- declare_sampling(n = n)
assignment <- declare_assignment(m = m)

# Answer Strategy --------------------------------------------------------------
estimator <- declare_estimator(Y ~ Z, estimand = estimand)

# Design -----------------------------------------------------------------------
two_arm <- declare_design(population, potential_outcomes, sampling, 
													estimand, assignment, reveal_outcomes, estimator)

## ----eval = FALSE, echo = FALSE------------------------------------------
#  diagnosis <- diagnose_design(two_arm, sims = 10000, bootstrap_sims = 1000)
#  saveRDS(diagnosis,"two_arm_diagnosis.RDS")

## ----eval = FALSE, echo = TRUE-------------------------------------------
#  diagnosis <- diagnose_design(two_arm, sims = 10000, bootstrap_sims = 1000)

## ----echo = FALSE--------------------------------------------------------
if(file.exists("two_arm_diagnosis.RDS")) diagnosis <- readRDS("two_arm_diagnosis.RDS")
if(exists("diagnosis")) {
  diagnosands <- get_diagnosands(diagnosis)
  diagnosis_table <- diagnosands[,c("mean_estimate","mean_estimand","bias","se(bias)","power","se(power)","coverage","se(coverage)")]
  names(diagnosis_table) <- c("Mean Estimate", "Mean Estimand", "Bias", "SE(Bias)", "Power", "SE(Power)", "Coverage", "SE(Coverage)")
  
  mean_estimate <- round(diagnosands[1,"mean_estimate"],3)
  mean_estimand <- round(diagnosands[1,"mean_estimand"],3)
  bias <- round(diagnosands[1,"bias"],3)
  power <- round(diagnosands[1,"power"],2)*100
  coverage <- round(diagnosands[1,"coverage"],2)*100
  
  knitr::kable(diagnosis_table,digits = 3)
}

