###############################################################################
# Lung Cancer Baseline Hazard Calibration Script
###############################################################################

# ========================================
# SETUP
# ========================================
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(MASS)
library(parallel)
library(DemosMiscanLung)

rm(list = ls())
gc()

if (!endsWith(getwd(), "modules/lc_baseline_hazard/calibration/simulation")) {
  setwd("modules/lc_baseline_hazard/calibration/simulation")
}

# ========================================
# CONFIGURATION
# ========================================
config <- jsonlite::fromJSON("../params/config.json")

PROP <- 1.0
GROUP <- "female"
TARGET_TYPE <- "incidence"
TARGET_YEAR <- c(2009:2019)

# ========================================
# LOAD DATA
# ========================================
STCC_targets <- readRDS("../data/STCC_ind_mort_target.rds")
target_ind_male <- STCC_targets$Incidence[["Male"]] %>%
  filter(year %in% TARGET_YEAR)
target_ind_female <- STCC_targets$Incidence[["Female"]] %>%
  filter(year %in% TARGET_YEAR)
pop_male <- STCC_targets$Incidence$Male %>%
  filter(year == TARGET_YEAR) %>%
  pull(population)
pop_female <- STCC_targets$Incidence$Female %>%
  filter(year == TARGET_YEAR) %>%
  pull(population)

# ========================================
# OBJECTIVE FUNCTION
# ========================================
objective_function <- function(lc_params, verbose = FALSE) {
  # Load environment/data once (or ensure it's loaded)
  suppressWarnings({
    source("load.R")
  })

  # Update parameters based on optimization input
  if (GROUP == "female") {
    lc_baseline_risk_params[4] <- lc_params[2] / 10
    lc_baseline_risk_params[3] <- lc_params[1] - 7 * lc_params[2]
  } else {
    lc_baseline_risk_params[2] <- lc_params[2] / 10
    lc_baseline_risk_params[1] <- lc_params[1] - 7 * lc_params[2]
  }

  if (verbose) {
    if (GROUP == "female") {
      cat(sprintf(
        "\n[Iteration] gamma0=%.6f, gamma1=%.6f, alpha0=%.6f, alpha1=%.6f\n",
        lc_baseline_risk_params[3], lc_baseline_risk_params[4], lc_params[1], lc_params[2]
      ))
    } else {
      cat(sprintf(
        "\n[Iteration] gamma0=%.6f, gamma1=%.6f, alpha0=%.6f, alpha1=%.6f\n",
        lc_baseline_risk_params[1], lc_baseline_risk_params[2], lc_params[1], lc_params[2]
      ))
    }
  }

  # Function to run a single simulation instance
  # This will be distributed to cores
  run_simulation_instance <- function(seed_offset) {
    # Set unique seed for this run to ensure stochasticity
    set.seed(config$configuration$num_seed + seed_offset)

    screen <- as.integer(strategy_list[1, 2])
    strategy_cond <- as.numeric(strategy_list[1, -c(1:2)])

    # Create local population copy for this worker
    population_local <- totpopulation %>% left_join(mapping, by = c("race", "gender"))
    population_local$year_immigrated[is.na(population_local$year_immigrated)] <- 0

    yearstop <- 1995
    t_age <- 90
    startrow <- 0
    conditionnumber <- 20
    smoking_ban_policy_index <- 0

    incidenceindiv <- matrix(0, yearstop + t_age - 1989, 176)
    mortindiv <- matrix(0, yearstop + t_age - 1989, 176)

    # Set globals for C++ access within this forked process
    yearborn <<- population_local$year_of_birth
    yearimmigrated <<- as.integer(population_local$year_immigrated)
    index <<- population_local$index
    beforeconditions <- NULL

    tryCatch(
      {
        while (startrow < nrow(population_local)) {
          afterconditions <- schedule_population_pre2025(
            ncol = conditionnumber,
            begin = startrow,
            end = nrow(population_local),
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
            verbose = FALSE # Silence internal printing for parallel runs
          )

          incidenceindiv <- incidenceindiv + incidencematrix
          mortindiv <- mortindiv + mortmatrix

          beforeconditions <- rbind(beforeconditions, afterconditions)
          if (is.null(birthmatrix) || sum(birthmatrix) == 0) break

          startrow <- nrow(population_local)
          population_local <- addpopulation(birthmatrix, yearstop) %>% rbind(population_local, .)

          # Update globals for next iteration in this worker
          yearborn <<- population_local$year_of_birth
          yearimmigrated <<- population_local$year_immigrated
          index <<- population_local$index

          invisible(gc())
        }

        results <- micro_cancer_burden(incidenceindiv, mortindiv)

        ind <- results$Incidence %>%
          filter(year %in% TARGET_YEAR) %>%
          group_by(year, gender, AgeGroup) %>%
          summarise(counts = sum(count), .groups = "drop")

        mort <- results$Mortality %>%
          filter(year %in% TARGET_YEAR) %>%
          group_by(year, gender, AgeGroup) %>%
          summarise(counts = sum(count), .groups = "drop")

        if (GROUP == "female") {
          output_ind <- ind %>%
            filter(gender == 1) %>%
            pull(counts)
          output_mort <- mort %>%
            filter(gender == 1) %>%
            pull(counts)
          target_v_ind <- target_ind_female$incidence
        } else {
          output_ind <- ind %>%
            filter(gender == 0) %>%
            pull(counts)
          output_mort <- mort %>%
            filter(gender == 0) %>%
            pull(counts)
          target_v_ind <- target_ind_male$incidence
        }

        # Calculate metric for this specific run
        # Using pmax(..., 1) to avoid division by near-zero which causes instability
        rmse_ind <- sum((output_ind - target_v_ind)^2 / pmax(target_v_ind, 1.0))
        rmse_mort <- 0

        single_obj <- if (TARGET_TYPE == "incidence") {
          rmse_ind
        } else if (TARGET_TYPE == "mortality") {
          rmse_mort
        } else {
          sqrt(0.5 * rmse_ind^2 + 0.5 * rmse_mort^2)
        }

        return(single_obj)
      },
      error = function(e) {
        if (verbose) cat("Sim Error:", e$message, "\n")
        return(NA)
      }
    )
  }

  # Run 5 simulations in parallel
  # mclapply uses forking on Mac/Linux, which is efficient
  num_simulations <- 10
  results_list <- mclapply(1:num_simulations, run_simulation_instance, mc.cores = num_simulations)

  # Filter valid results
  valid_metrics <- unlist(results_list)
  valid_metrics <- valid_metrics[!is.na(valid_metrics)]

  if (length(valid_metrics) == 0) {
    return(1e10)
  }

  # Average the objective values
  # This implicitly penalizes variance between runs: E[Error^2] = Bias^2 + Variance
  final_objective <- mean(valid_metrics)

  if (verbose) {
    cat(sprintf(
      paste0("Mean Objective (", num_simulations, " runs): %.4f [StdDev: %.4f]\n"),
      final_objective, sd(valid_metrics)
    ))
  }

  final_objective
}

# ========================================
# OPTIMIZATION
# ========================================
cat(sprintf(
  "\n%s\nOptimizing for %s - %s\n%s\n",
  strrep("=", 80), toupper(GROUP), TARGET_TYPE, strrep("=", 80)
))

initial_params <- if (GROUP == "male") {
  c(-7.914738, 0.912197)
} else {
  c(-6.772865, 0.845188)
}

cat(sprintf(
  "\nInitial: [%.6f, %.6f]\n",
  initial_params[1], initial_params[2]
))

initial_obj <- objective_function(initial_params, verbose = TRUE)
cat(sprintf("Initial objective: %.6f\n\n", initial_obj))

start_time <- Sys.time()

opt_result <- optim(
  par = initial_params,
  fn = function(p) objective_function(p, verbose = TRUE),
  method = "Nelder-Mead",
  control = list(maxit = 1000, pgtol = 1e-8, trace = 1, REPORT = 10)
)


end_time <- Sys.time()

# ========================================
# RESULTS
# ========================================
cat(sprintf("\n%s\nRESULTS\n%s\n", strrep("=", 80), strrep("=", 80)))
cat(sprintf("Time: %.1f minutes\n", difftime(end_time, start_time, units = "mins")))
cat(sprintf("Convergence: %d\n", opt_result$convergence))
cat(sprintf("Iterations: %d\n", opt_result$counts[1]))
cat(sprintf(
  "\nOptimal: [%.6f, %.6f]\n",
  opt_result$par[1], opt_result$par[2]
))
cat(sprintf("Objective: %.6f\n", opt_result$value))
cat(sprintf(
  "Improvement: %.2f%%\n",
  (initial_obj - opt_result$value) / initial_obj * 100
))

saveRDS(opt_result, file = sprintf(
  "../outputs/lc_baseline_hazard_calibration_%s_%s_result.rds",
  GROUP, TARGET_TYPE
))

###############################################################################
# END OF SCRIPT
###############################################################################
