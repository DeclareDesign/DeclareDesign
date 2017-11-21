## ----MIDA, echo = FALSE,include = FALSE----------------------------------
library(DeclareDesign)
library(dplyr)
library(ggplot2)

two_way_factorial_template <- function(N = c(30, 100, 500, 1000),
                             beta_A = c(1, -1, 0),
                             beta_B = c(-1, 0, 1),
                             beta_AB = c(.5, -1, -.5, 0, 1))
{
  N <- as.numeric(N[1])
  beta_A <- as.numeric(beta_A[1])
  beta_B <- as.numeric(beta_B[1])
  beta_AB <- as.numeric(beta_AB[1])

  # Model ------------------------------------------------------------------------
  population <- declare_population(N = N, noise = rnorm(N))
  
  potential_outcomes <- declare_potential_outcomes(
    Y_Z_T1 = noise,
    Y_Z_T2 = noise + beta_A,
    Y_Z_T3 = noise + beta_B,
    Y_Z_T4 = noise + beta_A + beta_B + beta_AB)
  
  # Inquiry ----------------------------------------------------------------------
  estimand <- declare_estimand(
    interaction = mean((Y_Z_T4 + Y_Z_T1) - (Y_Z_T2 + Y_Z_T3)), 
    label = "interaction")
  
  # Data Strategy ----------------------------------------------------------------
  assignment <- declare_assignment(num_arms = 4)
  
  # Answer Strategy --------------------------------------------------------------
  estimator <- declare_estimator(Y ~ A + B + A:B,
                                 model = lm_robust,
                                 coefficient_name = "A:B", 
                                 estimand = estimand)
  
  # Design -----------------------------------------------------------------------
  design <- declare_design(
    population,
    potential_outcomes,
    estimand,
    assignment,
    dplyr::mutate(A = as.numeric(Z %in% c("T2", "T4")),
                  B = as.numeric(Z %in% c("T3", "T4"))),
    reveal_outcomes,
    estimator)
    
  design
}
attr(two_way_factorial_template, "tips") <- c(
  "N" = "Size of population",
  "beta_A" = "Main effect of A",
  "beta_B" = "Main effect of B",
  "beta_AB" = "Interaction effect of A and B"
)

## ------------------------------------------------------------------------
# Set design parameters --------------------------------------------------------
N <- 1000
beta_A <- 0
beta_B <- 0
beta_AB <- .25

# Model ------------------------------------------------------------------------
population <- declare_population(N = N, noise = rnorm(N))

potential_outcomes <- declare_potential_outcomes(
  Y_Z_T1 = noise,
  Y_Z_T2 = noise + beta_A,
  Y_Z_T3 = noise + beta_B,
  Y_Z_T4 = noise + beta_A + beta_B + beta_AB)

# Inquiry ----------------------------------------------------------------------
estimand <- declare_estimand(
  interaction = mean((Y_Z_T4 + Y_Z_T1) - (Y_Z_T2 + Y_Z_T3)), 
  label = "interaction")

# Data Strategy ----------------------------------------------------------------
assignment <- declare_assignment(num_arms = 4)

# Answer Strategy --------------------------------------------------------------
estimator <- declare_estimator(Y ~ A + B + A:B,
                               model = lm_robust,
                               coefficient_name = "A:B", 
                               estimand = estimand)

# Design -----------------------------------------------------------------------
design <- declare_design(
  population,
  potential_outcomes,
  estimand,
  assignment,
  dplyr::mutate(A = as.numeric(Z %in% c("T2", "T4")),
                B = as.numeric(Z %in% c("T3", "T4"))),
  reveal_outcomes,
  estimator)

## ----eval = FALSE, echo = FALSE------------------------------------------
#  designs <- quick_design(two_way_factorial_template,
#                          N = seq(150,3000,150),
#                          beta_A = 0,
#                          beta_B = 0,
#                          beta_AB = .25)
#  diagnoses_N <- diagnose_design(designs, sims = 1000, bootstrap = FALSE)
#  diagnosis <- diagnose_design(design, sims = 1000, bootstrap_sims = 1000)
#  saveRDS(diagnoses_N,"two_way_factorial_diagnoses_N.RDS")
#  saveRDS(diagnosis,"two_way_factorial_diagnosis.RDS")

## ----eval = FALSE, echo = TRUE-------------------------------------------
#  diagnosis <- diagnose_design(design, sims = 1000, bootstrap_sims = 1000)

## ----echo = FALSE--------------------------------------------------------
if(file.exists("two_way_factorial_diagnosis.RDS")) diagnosis <- readRDS("two_way_factorial_diagnosis.RDS")
if(file.exists("two_way_factorial_diagnoses_N.RDS")) diagnoses_N <- readRDS("two_way_factorial_diagnoses_N.RDS")
if(exists("diagnosis")) {
  cols <- c("Mean Estimate"="mean_estimate",
           "Mean Estimand"="mean_estimand",
           "Bias"="bias",
           "SE(bias)"="se(bias)",
           "Power"="power",
           "SE(Power)"="se(power)",
           "Coverage"="coverage",
           "SE(Coverage)"="se(coverage)"
  )
  diagnosis_table <- get_diagnosands(diagnosis)[cols]
  diagnosands <- get_diagnosands(diagnosis)
  names(diagnosis_table) <- names(cols)
  
  mean_estimate <- round(diagnosands[1,"mean_estimate"],3)
  mean_estimand <- round(diagnosands[1,"mean_estimand"],3)
  bias <- round(diagnosands[1,"bias"],3)
  power <- round(diagnosands[1,"power"],2)*100
  coverage <- round(diagnosands[1,"coverage"],2)*100
  
  knitr::kable(diagnosis_table,digits = 3)
}

## ----eval = FALSE, echo = TRUE-------------------------------------------
#  designs <- quick_design(two_way_factorial_template,
#                          N = seq(150,3000,150),
#                          beta_A = 0,
#                          beta_B = 0,
#                          beta_AB = .25)
#  diagnoses_N <- diagnose_design(designs, sims = 1000, bootstrap = FALSE)

## ---- echo = FALSE-------------------------------------------------------
if(exists("diagnoses_N")) {
diagnosands_N <- get_diagnosands(diagnoses_N) 
min_sample <- min(diagnosands_N$N[diagnosands_N$power>.8])
diagnosands_N %>% 
ggplot(
	aes(
		y = power, 
		x = N
	)
) +
geom_point(color = "#C67800") +
geom_smooth(se = FALSE, size = .5, color = "#205C8A", method = "loess") +
scale_y_continuous(name = "Power",limits = c(0,1.1),breaks = c(seq(0,1,.25),.8),minor_breaks = NULL) +
scale_x_continuous(name = "N",breaks = c(seq(0,3000,500),min_sample)) + 
geom_hline(yintercept = .8,size = .1) +
geom_vline(xintercept = min_sample,size = .1) +
   theme_bw() +
      theme(
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(color = '#eeeeee'),
        strip.background = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Helvetica")
        )
}

