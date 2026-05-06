###############################################################################
# Smoking Calibration Script
###############################################################################

# ========================================
# PREPARATION
# ========================================
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(jsonlite)
library(MASS)

rm(list = ls())
gc()

# ========================================
# CONFIGURATION
# ========================================
if (!endsWith(getwd(), "modules/smoking/calibration/simulation")) {
  setwd("modules/smoking/calibration/simulation")
}

data_path <- "../data/"
params_path <- "../params/"
outputs_path <- "../outputs/"

params <- jsonlite::fromJSON(paste0(params_path, "config.json"))

num_seed <- params$configuration$`num_seed`
yearstop <- params$configuration$`yearstop`

conditionnumber <- params$simulation$`condition_number`
t_age <- params$simulation$`trunc_age`

# ========================================
# SIMULATION
# ========================================
set.seed(num_seed)

source("utilis.R")
Rcpp::sourceCpp("../core/impute-smoking.cpp")

smoking_params_posterior_samples <- readRDS("../params/smoking_params_posterior_samples.rds")
population <- readRDS("../data/baseline_pop.rds")
targets <- readRDS("../data/smoking_calibration_targets.rds")

# Group cohort
population <- population %>%
  mutate(
    yob_group = case_when(
      year_of_birth < 1935 ~ -7,
      TRUE ~ floor((year_of_birth - 1970) / 5)
    )
  )

# Generate matrices
yob_group <- population$yob_group
index <- population$index
yearborn <- population$year_of_birth

cat("Done preparation, start simulation!\n")

# ========================================
# CALIBRATION
# ========================================
cat("Starting calibration optimization...\n")

initial_params <- c(
  -0.08937899, -0.85877747, -1.50124157, 0.39044645, 0.25182579, -0.12064148, -1.43128159, -1.00001913, 1.56050791, 0.73669978
)

# N = 100

# best_params_vec <- matrix(0, nrow = N, ncol = length(initial_params))
# for (iter in 1:N) {
#   set.seed(iter)
#   smoking_params_posterior_samples_index <- sample.int(nrow(smoking_params_posterior_samples), 1)
#   smoking_params <- as.numeric(smoking_params_posterior_samples[smoking_params_posterior_samples_index, ])
#   calibration_result <- run_calibration(
#     initial_params = initial_params,
#     methods = c("Nelder-Mead"),
#     max_iter_bfgs = 1e4,
#     max_iter_nm = 1e5,
#     verbose = TRUE
#   )
#   best_params_vec[iter,] <- calibration_result$best_params
#   cat("==================Iteration ", iter, " completed.==================\n")
# }

smoking_params <- read.csv("../params/smoking_params.csv")[, 2] %>% as.matrix()
calibration_result <- run_calibration(
  initial_params = initial_params,
  methods = c("Nelder-Mead"),
  max_iter_bfgs = 1e4,
  max_iter_nm = 1e5,
  verbose = TRUE
)
best_params <- calibration_result$best_params

# ========================================
# VALIDATION
# ========================================
validation_result <- validate_calibration(
  calibration_params = best_params,
  targets = targets,
  t_age = t_age
)

# ========================================
# SAVE RESULTS
# ========================================
population_matrix <- schedule_population(
  N = nrow(population),
  smoking_params = smoking_params,
  calibration_params = best_params,
  trunc_age = t_age,
  verbose = FALSE
)

population$year_start_smoking <- population_matrix[, 1]
population$year_stop_smoking <- population_matrix[, 2]

micro_smoke_prevalence(population)

# Best parameters so far (RMSE: 1.448009)
# c(
#     -0.2045534, 1.4594067, -1.8103832, -1.8766527, 0.3751946,
#     -0.3223301, -1.3630391, -1.1531068, 1.4816520, 0.9480086
#   )
# saveRDS(best_params_vec, "../../../../params/calibration/smoking_calibration_params_samples.rds")
# saveRDS(best_params, "../../../../params/calibration/smoking_calibration_params.rds")

###############################################################################
# END OF SCRIPT
###############################################################################
