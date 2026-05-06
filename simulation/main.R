###############################################################################
# Main Simulation Script for DEMOS-MISCAN-Lung
###############################################################################

# ========================================
# PREPARATION
# ========================================
suppressPackageStartupMessages({
  library(plyr)
  library(dplyr)
  library(data.table)
  library(tidyr)
  library(jsonlite)
  library(MASS)
  library(DemosMiscanLung)
})

rm(list = ls())
invisible(gc())

# ========================================
# CONFIGURATION
# ========================================
if (!endsWith(getwd(), "simulation")) {
  setwd("simulation")
}

data_path <- "../data/"
params_path <- "../params/"
outputs_path <- "../outputs/"
sim_path <- paste0(outputs_path, "simulations/")
log_path <- paste0(outputs_path, "txts/")

args <- commandArgs(trailingOnly = TRUE)

params <- jsonlite::fromJSON(paste0(params_path, "config.json"))

if (length(args) > 0) {
  num_node <- as.integer(args[1])
  num_seed <- as.integer(args[2])
  strategy_no <- as.integer(args[3])
  smoking_ban_policy_index <- as.integer(args[4])
  cat(sprintf("Running strategy %d from command line in node %d\n", strategy_no, num_node))
} else {
  num_node <- params$configuration$`num_node`
  num_seed <- params$configuration$`num_seed`
  strategy_no <- params$configuration$`num_strategy`
  smoking_ban_policy_index <- params$configuration$`smoking ban policy index`
}

yearstop <- params$configuration$`yearstop`
startrow <- params$simulation$`start_row`
conditionnumber <- params$simulation$`condition_number`
t_age <- params$simulation$`trunc_age`
psa_flag <- params$psa$`flag`
screen_mode <- recode(params$configuration$screen_mode,
  without_fh = 0, with_fh = 1
)
sensitivity_lvl <- recode(params$configuration$sensitivity_lvl,
  low = 0, medium = 1, high = 2
)

# ========================================
# SIMULATION
# ========================================
start <- Sys.time()

suppressWarnings({
  source("load.R")
})

# Replace the lung parameters with PSA values
if (length(args) > 0) {
  # Random seed just for PSA sampling
  set.seed(as.integer(args[2]))

  # Smoking params draw
  smoking_params_posterior_samples <- readRDS(paste0(params_path, "smoking_params_posterior_samples.rds"))
  smoking_params_posterior_samples_index <- sample.int(nrow(smoking_params_posterior_samples), 1)
  smoking_params <- as.numeric(smoking_params_posterior_samples[smoking_params_posterior_samples_index, ])
  rm(smoking_params_posterior_samples, smoking_params_posterior_samples_index)

  # Scaling factor draw
  smoking_calibration_params_samples <- readRDS(paste0(params_path, "calibration/smoking_calibration_params_samples.rds"))
  smoking_calibration_params_index <- sample.int(nrow(smoking_calibration_params_samples), 1)
  smoking_calibration_params <- as.numeric(smoking_calibration_params_samples[smoking_calibration_params_index, ])

  # Lung cancer baseline hazard params draw
  lc_baseline_risk_params[1] <- rnorm(1, lc_baseline_risk_params[1], 0.0025)
  lc_baseline_risk_params[2] <- rnorm(1, lc_baseline_risk_params[2], 0.0025)
  lc_baseline_risk_params[3] <- rnorm(1, lc_baseline_risk_params[3], 0.0025)
  lc_baseline_risk_params[4] <- rnorm(1, lc_baseline_risk_params[4], 0.0025)

  # Lung cancer survival params draw
  lc_survival_params_vcov <- readRDS(paste0(params_path, "hessian/lc_survival_params_vcov.rds"))
  lc_survival_params <- mvrnorm(1, lc_survival_params, lc_survival_params_vcov)

  # Lung cancer treatment cost params draw
  ave_cost[, 1] <- rnorm(nrow(ave_cost), ave_cost[, 1], ave_cost[, 2])
}

# Reset global random seed
set.seed(num_seed)

log_file <- paste0(log_path, "log_node_", num_node, ".txt")

# Screen or not?
screen <- as.integer(strategy_list[strategy_no, 2])

# Set current strategy
strategy_cond <- as.numeric(strategy_list[strategy_no, -c(1:2)])

# Add race and gender to the baseline population with mapping as the reference
population <- totpopulation %>% left_join(mapping, by = c("race", "gender"))

# Create diagnose matrix
incidenceindiv <- matrix(0, yearstop + t_age - 1989, 176)
mortindiv <- matrix(0, yearstop + t_age - 1989, 176)

population$year_immigrated[is.na(population$year_immigrated)] <- 0
yearborn <- population$year_of_birth
yearimmigrated <- as.integer(population$year_immigrated)
index <- population$index
beforeconditions <- NULL

rm(totpopulation, immigrants_historical, immigrants_projection)
invisible(gc())

while (startrow < nrow(population)) {
  # Schedule conditions for current population
  afterconditions <- runSim(
    ncol = conditionnumber,
    begin = startrow,
    end = nrow(population),
    year_stop = yearstop,
    smoking_params = smoking_params,
    smoking_calibration_params = smoking_calibration_params,
    lc_baseline_risk_params = lc_baseline_risk_params,
    lc_dwell_weibull_params = lc_dwell_weibull_params,
    lc_transition_rates = lc_transition_rates,
    lc_survival_params = lc_survival_params,
    utility = utility_values,
    ave_cost = ave_cost,
    screen = screen,
    strategy_cond = strategy_cond,
    trunc_age = t_age,
    smoking_ban_policy_index = smoking_ban_policy_index,
    screen_mode = screen_mode,
    sensitivity_level = sensitivity_lvl,
    verbose = TRUE
  )

  beforeconditions <- rbind(beforeconditions, afterconditions)
  if (is.null(birthmatrix) || sum(birthmatrix) == 0) break

  # Add new births to population
  startrow <- nrow(population)
  population <- addpopulation(birthmatrix, yearstop) %>% rbind(population, .)
  yearborn <- population$year_of_birth
  yearimmigrated <- population$year_immigrated
  index <- population$index

  # Process diagnose matrix
  incidenceindiv <- incidenceindiv + incidencematrix
  mortindiv <- mortindiv + mortmatrix

  rm(mortmatrix, incidencematrix, birthmatrix)
  invisible(gc())
}

rm(afterconditions, yearborn, yearimmigrated, index)
invisible(gc())

beforeconditions <- as.data.frame(beforeconditions)
colnames(beforeconditions) <- c(
  "year_of_death", "year_start_smoking", "year_stop_smoking", "smoking_intensity",
  "year_lung_cancer", "Duration_I", "Duration_II", "Duration_III", "Duration_IV",
  "year_lung_cancer_diagnosed", "stage_diagnosed", "screening_detected",
  "Lung_specific", "Treatment_cost", "Type", "QALY",
  "Screen_rounds", "Screen_cost", "False_positive", "Family_history"
)

populationdata <- cbind(population, beforeconditions)
rm(population, beforeconditions)
invisible(gc())

# ========================================
# RESULTS
# ========================================
end <- Sys.time()

if (psa_flag & length(args) > 0) {
  cat(sprintf("Lung cancer PSA applied \n"))
  policy_name <- c("Baseline", "Mild", "Moderate", "Stringent", "Immediate_ban")

  # Save smoking prevalence
  tb <- micro_smoke_prevalence(populationdata, 18, 74, 2050)
  tb <- tb[-12, -1] %>%
    as.matrix() %>%
    as.numeric()
  tb %>% saveRDS(., paste0(
    outputs_path, "smoke_psa_output/run", "_seed_",
    as.integer(args[2]), "_policy_",
    policy_name[smoking_ban_policy_index + 1], ".rds"
  ))

  # Save lung cancer psa burden estimates
  matrix_list <- micro_cancer_burden(incidenceindiv, mortindiv)
  ind <- matrix_list$Incidence %>%
    filter(year <= 2050, year >= 2008, age_index < 12) %>%
    group_by(gender, year) %>%
    reframe(ind = sum(count))

  mort <- matrix_list$Mortality %>%
    filter(year <= 2050, year >= 2008, age_index < 12) %>%
    group_by(gender, year) %>%
    reframe(death = sum(count))

  left_join(ind, mort, by = c("gender", "year")) %>%
    list(
      ind_mort = .,
      Cost = sum(populationdata$Treatment_cost + populationdata$Screen_cost),
      QALY = sum(populationdata$QALY)
    ) %>%
    saveRDS(., paste0(
      outputs_path, "lc_psa_output/run", "_seed_",
      as.integer(args[2]), "_policy_",
      policy_name[smoking_ban_policy_index + 1], ".rds"
    ))
}

# Save the result
result <- list(
  "Cost" = sum(populationdata$Treatment_cost + populationdata$Screen_cost),
  "Treatment Cost" = sum(populationdata$Treatment_cost),
  "Screen Cost" = sum(populationdata$Screen_cost),
  "QALY" = sum(populationdata$QALY),
  "Life Years" = sum(ifelse(populationdata$year_of_death == 0, 90,
    pmin(90, populationdata$year_of_death - populationdata$year_of_birth)
  )),
  "Total Screens Required" = sum(populationdata$Screen_rounds),
  "Total number of screened people" = sum(populationdata$Screen_rounds > 0),
  "False Positives" = sum(populationdata$False_positive)
)

# Print out the log file to check if everything OK in this core
lines <- c(
  format_line_box(sprintf("Node %s  Seed %s  Strategy %s  Smoking_ban_policy %s", num_node, num_seed, strategy_no - 1, smoking_ban_policy_index)),
  format_line_box(""),
  format_line_box(sprintf("Time: %.3f hours", as.numeric(difftime(end, start, units = "hours")))),
  format_line_box(""),
  format_line_box(sprintf("Cost: %.2f", result$Cost)),
  format_line_box(sprintf("Treatment Cost: %.2f", result$`Treatment Cost`)),
  format_line_box(sprintf("Screen Cost: %.2f", result$`Screen Cost`)),
  format_line_box(sprintf("QALY: %.2f", result$QALY)),
  format_line_box(""),
  format_line_box(sprintf("Life Years: %.2f", result$`Life Years`)),
  format_line_box(sprintf("Total Screens Required: %d", result$`Total Screens Required`)),
  format_line_box(sprintf(
    "Total number of screened people: %d",
    result$`Total number of screened people`
  )),
  format_line_box(sprintf("False Positives: %d", result$`False Positives`)),
  format_line_box(""),
  format_line_box(sprintf(
    "Number of total cases: %d,  Male: %d,  Female: %d",
    sum(incidenceindiv[-c(1:35), ]),
    sum(incidenceindiv[-c(1:35), 1:88]),
    sum(incidenceindiv[-c(1:35), 89:176])
  )),
  format_line_box(sprintf(
    "Number of total screened: %d,  Male: %d,  Female: %d",
    sum(incidenceindiv[-c(1:35), unlist(lapply(
      seq(5, 173, 8),
      function(x) x:(x + 3)
    ))]),
    sum(incidenceindiv[-c(1:35), unlist(lapply(
      seq(5, 85, 8),
      function(x) x:(x + 3)
    ))]),
    sum(incidenceindiv[-c(1:35), unlist(lapply(
      seq(93, 173, 8),
      function(x) x:(x + 3)
    ))])
  )),
  format_line_box(sprintf(
    "Number of total cases trunc: %d,  Male: %d,  Female: %d",
    sum(incidenceindiv[-c(1:35), c(1:72, 89:160)]),
    sum(incidenceindiv[-c(1:35), 1:72]),
    sum(incidenceindiv[-c(1:35), 89:160])
  )),
  format_line_box(sprintf(
    "Number of total screened trunc: %d,  Male: %d,  Female: %d",
    sum(incidenceindiv[-c(1:35), unlist(lapply(
      c(
        seq(5, 69, 8),
        seq(93, 157, 8)
      ),
      function(x) x:(x + 3)
    ))]),
    sum(incidenceindiv[-c(1:35), unlist(lapply(
      seq(5, 69, 8),
      function(x) x:(x + 3)
    ))]),
    sum(incidenceindiv[-c(1:35), unlist(lapply(
      seq(93, 157, 8),
      function(x) x:(x + 3)
    ))])
  )),
  format_line_box(sprintf(
    "Number of late cases: %d,  Male: %d,  Female: %d",
    sum(incidenceindiv[-c(1:35), c(seq(3, 175, 4), seq(4, 176, 4))]),
    sum(incidenceindiv[-c(1:35), c(seq(3, 87, 4), seq(4, 88, 4))]),
    sum(incidenceindiv[-c(1:35), c(seq(91, 175, 4), seq(92, 176, 4))])
  )),
  format_line_box(sprintf(
    "Number of total deaths: %d,  Male: %d,  Female: %d",
    sum(mortindiv[-c(1:35), ]),
    sum(mortindiv[-c(1:35), 1:88]),
    sum(mortindiv[-c(1:35), 89:176])
  )),
  format_line_box(sprintf(
    "Number of late deaths: %d,  Male: %d,  Female: %d",
    sum(mortindiv[-c(1:35), c(seq(3, 175, 4), seq(4, 176, 4))]),
    sum(mortindiv[-c(1:35), c(seq(3, 87, 4), seq(4, 88, 4))]),
    sum(mortindiv[-c(1:35), c(seq(91, 175, 4), seq(92, 176, 4))])
  )),
  format_line_box(""),
  format_line_box(sprintf("Population size: %d", nrow(populationdata))),
  format_line_box(sprintf("Random number sequence check: %.4f", runif(1))),
  paste0("+", strrep("-", 68), "+")
)
print(end - start)
cat(paste(lines, collapse = "\n"), file = log_file, append = TRUE)
cat("\n", file = log_file, append = TRUE)
cat("Done for strategy", strategy_no, "\n")
cat("-----------------------------------------\n")

###############################################################################
# END OF SCRIPT
###############################################################################
