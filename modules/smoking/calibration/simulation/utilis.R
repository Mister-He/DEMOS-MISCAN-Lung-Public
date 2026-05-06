###############################################################################
# Utility Functions for Smoking Calibration
###############################################################################

# ========================================
# MISCELLANEOUS
# ========================================
mapping <- data.frame(
  race = rep(c("chinese", "malay", "indian", "other"), each = 2),
  gender = rep(c("male", "female"), 4),
  index = 0:7
)

construct <- function(ind, df) {
  if (df$count[ind] > 0) {
    data.frame(
      year_of_birth = df$year_of_birth[ind],
      race = "0",
      gender = "0",
      year_immigrated = 0,
      index = rep(df$index[ind], df$count[ind])
    )
  } else {
    NULL
  }
}

addpopulation <- function(birthmatrix, year_stop = 2030) {
  colnames(birthmatrix) <- 0:7
  df <- as.data.frame(birthmatrix) %>%
    mutate(year_of_birth = 1991:year_stop) %>%
    pivot_longer(
      cols = c(as.character(0:7)),
      names_to = "index",
      values_to = "count"
    )

  popdf <- do.call(rbind, lapply(1:nrow(df), construct, df = df)) %>%
    rows_update(
      mutate(mapping, index = as.character(0:7)),
      by = "index",
      unmatched = "ignore"
    ) %>%
    mutate(index = as.numeric(index))

  popdf
}

clean_matrix <- function(mtx) {
  mtx <- as.matrix(mtx)

  for (i in seq_len(nrow(mtx))) {
    row_vals <- mtx[i, ]

    if (is.na(row_vals[1])) {
      row_vals[1] <- 0
    }

    for (j in 2:length(row_vals)) {
      if (is.na(row_vals[j])) {
        row_vals[j] <- row_vals[j - 1]
      }
    }

    mtx[i, ] <- row_vals
  }

  mtx
}

# ========================================
# CALIBRATION FUNCTIONS
# ========================================
objective_function <- function(calib_params, verbose_obj = TRUE) {
  set.seed(1)

  population_matrix <- schedule_population(
    N = nrow(population),
    smoking_params = smoking_params,
    calibration_params = calib_params,
    trunc_age = t_age,
    verbose = FALSE
  )

  population$year_start_smoking <- population_matrix[, 1]
  population$year_stop_smoking <- population_matrix[, 2]

  fit_to_group_index <- c(1:11)

  estimates <- as.matrix(micro_smoke_prevalence(population)[fit_to_group_index, -1])
  logit_estimates <- qlogis(estimates / 100)

  weights <- 1 / ((targets$logit_upper[fit_to_group_index, ] -
    targets$logit_lower[fit_to_group_index, ]) / (2 * 1.96))^2
  objective <- sqrt(mean(((logit_estimates -
    targets$logit_mean[fit_to_group_index, ])^2) * weights))

  if (verbose_obj) {
    cat(
      "Objective:", round(objective, 8),
      "| Params:", paste(round(calib_params, 8), collapse = ", "), "\n"
    )
  }

  objective
}

run_calibration <- function(initial_params = rep(0, 10),
                            lower = rep(-3, 10),
                            upper = rep(3, 10),
                            methods = c("L-BFGS-B", "Nelder-Mead"),
                            max_iter_bfgs = 1e5,
                            max_iter_nm = 1e5,
                            verbose = TRUE) {
  start_time <- Sys.time()

  if (verbose) {
    cat("\n╔═══════════════════════════════════════════════════════════╗\n")
    cat("║           SMOKING CALIBRATION OPTIMIZATION                ║\n")
    cat("╚═══════════════════════════════════════════════════════════╝\n\n")
    cat("Testing objective function with initial parameters...\n")
  }

  initial_objective <- objective_function(initial_params, verbose_obj = verbose)

  if (verbose) {
    cat("Initial objective:", round(initial_objective, 4), "\n")
    cat("Population size:", nrow(population), "\n")
    cat("Parameter bounds: [", paste(lower[1], upper[1], sep = ", "), "]\n\n")
  }

  results_list <- list()
  comparison_data <- data.frame(
    Method = "Initial",
    Objective = initial_objective,
    Convergence = NA,
    Success = NA,
    Runtime_mins = 0,
    stringsAsFactors = FALSE
  )

  if ("BFGS" %in% methods) {
    if (verbose) {
      cat("═══════════════════════════════════════════════════════════\n")
      cat("Running L-BFGS-B Optimization\n")
      cat("═══════════════════════════════════════════════════════════\n")
    }

    start_bfgs <- Sys.time()

    result_bfgs <- tryCatch(
      {
        optim(
          par = initial_params,
          fn = objective_function,
          method = "L-BFGS-B",
          lower = lower,
          upper = upper,
          control = list(
            maxit = max_iter_bfgs,
            trace = ifelse(verbose, 1, 0),
            REPORT = 1000
          ),
          verbose_obj = TRUE
        )
      },
      error = function(e) {
        if (verbose) cat("ERROR in L-BFGS-B:", e$message, "\n")
        NULL
      }
    )

    end_bfgs <- Sys.time()
    runtime_bfgs <- as.numeric(difftime(end_bfgs, start_bfgs, units = "mins"))

    if (!is.null(result_bfgs)) {
      results_list$BFGS <- result_bfgs

      comparison_data <- rbind(comparison_data, data.frame(
        Method = "BFGS",
        Objective = result_bfgs$value,
        Convergence = result_bfgs$convergence,
        Success = result_bfgs$convergence == 0,
        Runtime_mins = runtime_bfgs,
        stringsAsFactors = FALSE
      ))

      if (verbose) {
        cat("\nL-BFGS-B Results:\n")
        cat(
          "  Convergence:", result_bfgs$convergence,
          ifelse(result_bfgs$convergence == 0, "✓ SUCCESS", "✗ FAILED"), "\n"
        )
        cat("  Final objective:", round(result_bfgs$value, 4), "\n")
        cat("  Runtime:", round(runtime_bfgs, 2), "minutes\n")
        cat(
          "  Improvement:", round(initial_objective - result_bfgs$value, 4),
          sprintf("(%.1f%%)\n", 100 * (initial_objective - result_bfgs$value) /
            initial_objective)
        )
      }
    }
  }

  if ("Nelder-Mead" %in% methods) {
    if (verbose) {
      cat("\n═══════════════════════════════════════════════════════════\n")
      cat("Running Nelder-Mead Optimization\n")
      cat("═══════════════════════════════════════════════════════════\n")
    }

    start_nm <- Sys.time()

    result_nm <- tryCatch(
      {
        optim(
          par = initial_params,
          fn = objective_function,
          method = "Nelder-Mead",
          control = list(
            maxit = max_iter_nm,
            reltol = 1e-8,
            trace = ifelse(verbose, 1, 0),
            REPORT = 1000
          ),
          verbose_obj = TRUE
        )
      },
      error = function(e) {
        if (verbose) cat("ERROR in Nelder-Mead:", e$message, "\n")
        NULL
      }
    )

    end_nm <- Sys.time()
    runtime_nm <- as.numeric(difftime(end_nm, start_nm, units = "mins"))

    if (!is.null(result_nm)) {
      results_list$`Nelder-Mead` <- result_nm

      comparison_data <- rbind(comparison_data, data.frame(
        Method = "Nelder-Mead",
        Objective = result_nm$value,
        Convergence = result_nm$convergence,
        Success = result_nm$convergence == 0,
        Runtime_mins = runtime_nm,
        stringsAsFactors = FALSE
      ))

      if (verbose) {
        cat("\nNelder-Mead Results:\n")
        cat(
          "  Convergence:", result_nm$convergence,
          ifelse(result_nm$convergence == 0, "✓ SUCCESS", "✗ FAILED"), "\n"
        )
        cat("  Final objective:", round(result_nm$value, 4), "\n")
        cat("  Runtime:", round(runtime_nm, 2), "minutes\n")
        cat(
          "  Improvement:", round(initial_objective - result_nm$value, 4),
          sprintf("(%.1f%%)\n", 100 * (initial_objective - result_nm$value) /
            initial_objective)
        )
      }
    }
  }

  if (verbose) {
    cat("\n═══════════════════════════════════════════════════════════\n")
    cat("Comparison of All Methods\n")
    cat("═══════════════════════════════════════════════════════════\n")
    print(comparison_data)
  }

  converged_rows <- which(comparison_data$Success == TRUE)

  if (length(converged_rows) > 0) {
    best_idx <- converged_rows[which.min(comparison_data$Objective[converged_rows])]
    if (verbose) {
      cat("\nBest converged method:", comparison_data$Method[best_idx], "✓\n")
    }
  } else {
    if (verbose) {
      cat("\nWARNING: No method converged successfully!\n")
      cat("Using method with lowest objective value.\n")
    }
    best_idx <- which.min(comparison_data$Objective[-1]) + 1
  }

  best_method <- comparison_data$Method[best_idx]
  best_params <- switch(best_method,
    "BFGS" = results_list$BFGS$par,
    "Nelder-Mead" = results_list$`Nelder-Mead`$par,
    initial_params
  )

  final_objective <- comparison_data$Objective[best_idx]
  improvement_pct <- 100 * (initial_objective - final_objective) / initial_objective

  end_time <- Sys.time()
  total_runtime <- as.numeric(difftime(end_time, start_time, units = "mins"))

  cat("Optimal Calibration Parameters:\n")
  param_names <- c(
    "theta_init_male", "beta_init_male", "theta_cess_male", "beta_cess_male",
    "theta_init_female", "beta_init_female", "theta_cess_female", "beta_cess_female",
    "theta_init_mal_offset", "beta_init_mal_offset"
  )

  param_df <- data.frame(
    Parameter = param_names,
    Value = round(best_params, 6)
  )
  print(param_df, row.names = FALSE)
  cat("\n")

  if (verbose) {
    cat("\n╔═══════════════════════════════════════════════════════════╗\n")
    cat("║                    FINAL RESULTS                          ║\n")
    cat("╠═══════════════════════════════════════════════════════════╣\n")
    cat(sprintf("║ Best Method:         %-36s ║\n", best_method))
    cat(sprintf("║ Initial Objective:   %-36.4f ║\n", initial_objective))
    cat(sprintf("║ Final Objective:     %-36.4f ║\n", final_objective))
    cat(sprintf("║ Improvement:         %-34.3f%% ║\n", improvement_pct))
    cat(sprintf("║ Total Runtime:       %-33.1f min ║\n", total_runtime))
    cat("╚═══════════════════════════════════════════════════════════╝\n\n")
  }

  list(
    best_params = best_params,
    best_method = best_method,
    comparison = comparison_data,
    all_results = results_list,
    initial_objective = initial_objective,
    final_objective = final_objective,
    improvement_pct = improvement_pct,
    runtime_mins = total_runtime,
    convergence_success = comparison_data$Success[best_idx],
    param_names = param_names
  )
}

validate_calibration <- function(calibration_params, targets, t_age) {
  cat("\n═══════════════════════════════════════════════════════════\n")
  cat("Validating Calibration Results\n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  population_matrix <- schedule_population(
    N = nrow(population),
    smoking_params = smoking_params,
    calibration_params = calibration_params,
    trunc_age = t_age,
    verbose = FALSE
  )

  population$year_start_smoking <- population_matrix[, 1]
  population$year_stop_smoking <- population_matrix[, 2]

  estimates <- as.matrix(micro_smoke_prevalence(population)[1:8, -1])
  target_means <- targets$Mean[1:8, ]

  raw_diff <- as.matrix(estimates - target_means)

  mae <- mean(abs(raw_diff))
  rmse <- sqrt(mean(raw_diff^2))
  max_error <- max(abs(raw_diff))

  cat("Fit Quality Metrics:\n")
  cat(sprintf("  MAE:        %.2f%%\n", mae))
  cat(sprintf("  RMSE:       %.2f%%\n", rmse))
  cat(sprintf("  Max Error:  %.2f%%\n", max_error))

  quality <- if (mae < 2 && rmse < 3) {
    "EXCELLENT ✓✓✓"
  } else if (mae < 3 && rmse < 4) {
    "GOOD ✓✓"
  } else if (mae < 5 && rmse < 6) {
    "ACCEPTABLE ✓"
  } else {
    "NEEDS IMPROVEMENT ✗"
  }

  cat(sprintf("\nOverall Quality: %s\n", quality))

  list(
    estimates = estimates,
    targets = target_means,
    differences = raw_diff,
    mae = mae,
    rmse = rmse,
    max_error = max_error,
    quality = quality
  )
}

# ========================================
# SMOKING PREVALENCE FUNCTION
# ========================================
micro_smoke_prevalence <- function(populationdata,
                                   minage = 18,
                                   maxage = 74,
                                   year_stop = 2024) {
  interest_yrs <- c(1998, 2019:year_stop)
  interest_yrs <- interest_yrs[interest_yrs <= year_stop]
  prevalencetable <- matrix(0, nrow = 12, ncol = length(interest_yrs))

  age_groups <- c("18-29", "30-39", "40-49", "50-59", "60-74")

  for (year in interest_yrs) {
    peoplealive <- populationdata %>%
      filter(
        year - year_of_birth >= 18,
        year - year_of_birth <= maxage,
        year_of_death >= year | year_of_death == 0,
        year_immigrated <= year
      ) %>%
      mutate(age = year - year_of_birth) %>%
      mutate(age = cut(
        age,
        breaks = c(18, 29, 39, 49, 59, 74),
        include.lowest = TRUE,
        labels = age_groups
      ))

    peoplealive_with_smoking <- peoplealive %>%
      filter(
        year_start_smoking > 0,
        year_start_smoking <= year,
        (year_start_smoking > year_stop_smoking | year_stop_smoking >= year)
      )

    prevalencetable[1, which(interest_yrs == year)] <-
      nrow(peoplealive_with_smoking) / nrow(peoplealive) * 100

    prevalencetable[2:6, which(interest_yrs == year)] <-
      peoplealive_with_smoking %>%
      group_by(age) %>%
      reframe(n = n()) %>%
      complete(age = age_groups, fill = list(n = 0)) %>%
      mutate(prevalence = n / (
        peoplealive %>%
          group_by(age) %>%
          reframe(n = n()) %>%
          complete(age = age_groups, fill = list(n = 0)) %>%
          pull(n)
      ) * 100) %>%
      pull(prevalence)

    prevalencetable[7:8, which(interest_yrs == year)] <-
      peoplealive_with_smoking %>%
      group_by(gender) %>%
      reframe(n = n()) %>%
      complete(gender = unique(populationdata$gender), fill = list(n = 0)) %>%
      mutate(prevalence = n / (
        peoplealive %>%
          group_by(gender) %>%
          reframe(n = n()) %>%
          complete(gender = unique(populationdata$gender), fill = list(n = 0)) %>%
          pull(n)
      ) * 100) %>%
      pull(prevalence)

    prevalencetable[9:12, which(interest_yrs == year)] <-
      peoplealive_with_smoking %>%
      group_by(race) %>%
      reframe(n = n()) %>%
      complete(race = unique(populationdata$race), fill = list(n = 0)) %>%
      mutate(prevalence = n / (
        peoplealive %>%
          group_by(race) %>%
          reframe(n = n()) %>%
          complete(race = unique(populationdata$race), fill = list(n = 0)) %>%
          pull(n)
      ) * 100) %>%
      pull(prevalence)
  }

  colnames(prevalencetable) <- interest_yrs

  prevalencetable <- prevalencetable %>%
    as.data.frame() %>%
    tibble::add_column(
      Group = c(
        "Total", "18-29", "30-39", "40-49", "50-59", "60-74",
        "Female", "Male", "Chinese", "Indian", "Malay", "Other"
      ),
      .before = 1
    ) %>%
    .[-nrow(.), ]

  prevalencetable
}

###############################################################################
# END OF SCRIPT
###############################################################################
