###############################################################################
# Cause-Specific Gompertz PH Model: Lung Cancer Survival (Individual-level)
# - Expand cohort tables -> individual data
# - Fit Gompertz PH: Surv(time,event) ~ stage + smoke + dx_year_c
# - Plot Observed (KM 95%CI) vs Estimated (Model 95%CI)
###############################################################################
rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(survival)
  library(flexsurv)
  library(ggplot2)
  library(MASS)
  library(tibble)
})

# ========================================
# 0. USER SETTINGS
# ========================================
if (!endsWith(getwd(), "modules/lc_survival/training")) {
  setwd("modules/lc_survival/training")
}
DATA_DIR <- "data"
OUT_DIR <- "outputs"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# plot settings
T_MAX <- 15 # max years after diagnosis to plot
DT <- 1 # time step for smooth model curve
MIN_N <- 1 # minimum N per subgroup to make a facet
DX_YEAR_CENTER <- 2015

# years to include in facets (too many years -> huge figure)
# if you want all years present in the data, set YEARS_TO_PLOT <- NULL
YEARS_TO_PLOT <- NULL # e.g., 2008:2021
set.seed(1)

# ========================================
# 1. COHORT EXPANSION FUNCTION
# ========================================
expand_cohort <- function(path) {
  data <- read.csv(path, check.names = FALSE)

  # replace 0 and NA with NA (treat as missing follow-up / no info)
  data[data == 0 | is.na(data)] <- NA

  # df: rows = follow-up time (0..K), cols = dx_year
  df <- data.frame(t(data[, -1, drop = FALSE]))
  colnames(df) <- data[[1]]

  out <- list()
  id_counter <- 1
  years <- colnames(df)

  for (j in years) {
    N <- df[[j]]

    # if everything missing, skip
    if (all(is.na(N))) next

    # max_k = last non-NA row index - 1 (because row 1 corresponds to time 0)
    last_idx <- max(which(!is.na(N)))
    max_k <- last_idx - 1

    # if max_k <= 0, then no intervals to form (but may still have survivors at time 0)
    # events in each interval: D(k) = N(k) - N(k+1), event time recorded as k+1
    if (max_k >= 1) {
      for (k in 0:(max_k - 1)) {
        d <- N[k + 1] - N[k + 2]
        if (!is.na(d) && d > 0) {
          out[[length(out) + 1]] <- data.frame(
            id      = id_counter:(id_counter + d - 1),
            time    = runif(d, k, k + 1),
            status  = "LC",
            dx_year = as.integer(j)
          )
          id_counter <- id_counter + d
        }
      }
    }

    # administrative censoring at max_k with survivors N(max_k)
    c <- N[max_k + 1]
    if (!is.na(c) && c > 0) {
      # place censoring time within the last observed interval
      lo <- max(0, max_k - 1)
      hi <- max_k
      out[[length(out) + 1]] <- data.frame(
        id      = id_counter:(id_counter + c - 1),
        time    = hi, # runif(c, lo, hi),
        status  = "censor",
        dx_year = as.integer(j)
      )
      id_counter <- id_counter + c
    }
  }

  bind_rows(out)
}

# ========================================
# 2. LOAD + EXPAND ALL 8 TABLES
# ========================================
nosmoke_I <- expand_cohort(file.path(DATA_DIR, "nosmoke_I.csv")) %>% mutate(stage = 1, smoke = 0)
nosmoke_II <- expand_cohort(file.path(DATA_DIR, "nosmoke_II.csv")) %>% mutate(stage = 2, smoke = 0)
nosmoke_III <- expand_cohort(file.path(DATA_DIR, "nosmoke_III.csv")) %>% mutate(stage = 3, smoke = 0)
nosmoke_IV <- expand_cohort(file.path(DATA_DIR, "nosmoke_IV.csv")) %>% mutate(stage = 4, smoke = 0)

smoke_I <- expand_cohort(file.path(DATA_DIR, "smoke_I.csv")) %>% mutate(stage = 1, smoke = 1)
smoke_II <- expand_cohort(file.path(DATA_DIR, "smoke_II.csv")) %>% mutate(stage = 2, smoke = 1)
smoke_III <- expand_cohort(file.path(DATA_DIR, "smoke_III.csv")) %>% mutate(stage = 3, smoke = 1)
smoke_IV <- expand_cohort(file.path(DATA_DIR, "smoke_IV.csv")) %>% mutate(stage = 4, smoke = 1)

output <- bind_rows(
  nosmoke_I, nosmoke_II, nosmoke_III, nosmoke_IV,
  smoke_I, smoke_II, smoke_III, smoke_IV
)

# saveRDS(output, file.path(DATA_DIR, "expanded_cohort.rds"))

# ========================================
# 3. PREPARE DATA + FIT ONE STRATIFIED GOMPERTZ PH MODEL
# ========================================
df <- output %>%
  mutate(
    event     = as.integer(status == "LC"),
    dx_year   = as.integer(dx_year),
    dx_year_c = dx_year - DX_YEAR_CENTER,
    stage     = factor(stage), # reference = stage 1
    smoke     = factor(smoke, levels = c(0, 1)) # reference = non-smoker
  ) %>%
  filter(time > 0, !is.na(event), !is.na(stage), !is.na(smoke), !is.na(dx_year_c))

cat("\nFitting Single Stratified Model (Interactions with Stage)...\n")

# One model where:
# - Rate parameter has stage-specific intercept + stage-specific effects for smoke & year
# - Shape parameter also varies by stage
fit_stratified <- flexsurvreg(
  Surv(time, event) ~ stage + smoke + dx_year_c,
  anc  = list(shape = ~stage),
  data = df %>% filter(dx_year <= 2019),
  dist = "gompertz"
)

cat("\n===== Stratified Gompertz PH fit =====\n")
print(fit_stratified)

# ========================================
# 4. EXPORT COEFFICIENT SUMMARY (fit_stratified)
# ========================================
vcov <- vcov(fit_stratified)
coef_table <- fit_stratified$coefficients %>%
  as.data.frame() %>%
  tibble::rownames_to_column("param")

# saveRDS(H, file.path(OUT_DIR, "lc_survival_params_vcov.rds"))
# write.csv(coef_table, file.path(OUT_DIR, "lc_survival_params.csv"), row.names = FALSE)

# ========================================
# 5. BUILD OBSERVED KM (95%CI) BY (dx_year, stage, smoke)
# ========================================
make_km_df <- function(sub_df) {
  sub_df <- mutate(sub_df, time = ceiling(time))
  sf <- survfit(Surv(time, event) ~ 1, data = sub_df)

  km <- data.frame(
    time       = sf$time,
    S_obs      = sf$surv,
    S_obs_low  = sf$lower,
    S_obs_high = sf$upper
  )

  rbind(
    data.frame(time = 0, S_obs = 1, S_obs_low = 1, S_obs_high = 1),
    km
  )
}

# ========================================
# 6. BUILD MODEL ESTIMATED SURVIVAL USING fit_stratified
# ========================================
make_model_df <- function(stage_i, smoke_i, year_i, t_max = T_MAX, dt = DT) {
  tt <- seq(0, t_max, by = dt)

  nd <- data.frame(
    stage     = factor(stage_i, levels = levels(df$stage)),
    smoke     = factor(smoke_i, levels = levels(df$smoke)),
    dx_year_c = as.integer(year_i) - DX_YEAR_CENTER
  )

  s <- summary(
    fit_stratified,
    newdata = nd,
    type = "survival",
    t = tt,
    ci = TRUE
  )[[1]]

  data.frame(
    time   = s$time,
    S_hat  = s$est,
    S_low  = s$lcl,
    S_high = s$ucl
  )
}

# ========================================
# 7. PLOTTING: Observed vs Estimated (95%CI)
# ========================================
plot_survival_by_year_stage_smoke <- function(stage_i, smoke_i, years_vec) {
  plot_list <- list()

  for (yy in years_vec) {
    sub <- df %>%
      filter(
        stage == factor(stage_i, levels = levels(df$stage)),
        smoke == smoke_i,
        dx_year == yy
      )

    if (nrow(sub) < MIN_N) next

    km_df <- make_km_df(sub) %>%
      mutate(
        dx_year = yy,
        stage   = factor(stage_i, levels = levels(df$stage)),
        smoke   = smoke_i
      )

    mod_df <- make_model_df(stage_i, smoke_i, yy) %>%
      mutate(
        dx_year = yy,
        stage   = factor(stage_i, levels = levels(df$stage)),
        smoke   = smoke_i
      )

    plot_list[[length(plot_list) + 1]] <- list(km = km_df, mod = mod_df)
  }

  if (length(plot_list) == 0) {
    warning(sprintf("No valid groups to plot for stage=%s smoke=%s", stage_i, smoke_i))
    return(NULL)
  }

  km_all <- bind_rows(lapply(plot_list, `[[`, "km")) %>% filter(time <= T_MAX)
  mod_all <- bind_rows(lapply(plot_list, `[[`, "mod")) %>% filter(time <= T_MAX)

  p <- ggplot() +
    # Model ribbon + line
    geom_ribbon(
      data = mod_all,
      aes(x = time, ymin = S_low, ymax = S_high),
      fill = "red", alpha = 0.18
    ) +
    geom_line(
      data = mod_all,
      aes(x = time, y = S_hat),
      color = "red", linewidth = 0.8
    ) +
    # Observed KM CI + step curve
    geom_errorbar(
      data = km_all,
      aes(x = time, ymin = S_obs_low, ymax = S_obs_high),
      color = "blue", width = 0.1, alpha = 0.5
    ) +
    geom_point(
      data = km_all,
      aes(x = time, y = S_obs),
      color = "blue", size = 1
    ) +
    # # Observed KM CI + step curve
    # geom_ribbon(
    #   data = km_all,
    #   aes(x = time, ymin = S_obs_low, ymax = S_obs_high),
    #   fill = "blue", alpha = 0.12
    # ) +
    # geom_step(
    #   data = km_all,
    #   aes(x = time, y = S_obs),
    #   color = "blue", linewidth = 0.8
    # ) +
    facet_wrap(~dx_year, ncol = 4) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, T_MAX, by = 2)) +
    labs(
      title = sprintf(
        "Observed (KM) vs Estimated (Stratified Gompertz) | Stage=%s Smoke=%s",
        stage_i, smoke_i
      ),
      subtitle = sprintf(
        "Model: Surv ~ stage + stage:smoke + stage:dx_year_c; shape ~ stage. Center=%d. 95%% CI shown.",
        DX_YEAR_CENTER
      ),
      x = "Years after diagnosis",
      y = "Survival probability"
    ) +
    theme_bw(base_size = 12)
  return(list(plot = p, km_df = km_all, mod_df = mod_all))
}

# ========================================
# 8. GENERATE ALL PLOTS (stage x smoke)
# ========================================
all_years <- sort(unique(df$dx_year))
if (!is.null(YEARS_TO_PLOT)) {
  all_years <- intersect(all_years, YEARS_TO_PLOT)
}

plot_paths <- c()
output <- NULL

for (st in 1:4) {
  for (sm in 0:1) {
    p <- plot_survival_by_year_stage_smoke(stage_i = st, smoke_i = sm, years_vec = all_years)
    if (is.null(p)) next

    out_file <- file.path(OUT_DIR, sprintf("KM_vs_StratGompertz_stage%s_smoke%s.png", st, sm))
    print(p$plot)
    # ggsave(out_file, plot = p, width = 14, height = 9, dpi = 200)
    plot_paths <- c(plot_paths, out_file)

    p$km_df <- p$km_df %>% mutate(id = sprintf("dx_year%d_stage%d_smoke%d_time%d", dx_year, st, sm, p$km_df$time))
    p$mod_df <- p$mod_df %>% mutate(id = sprintf("dx_year%d_stage%d_smoke%d_time%d", dx_year, st, sm, p$mod_df$time))
    temp_df <- left_join(p$mod_df, p$km_df, by = "id", keep = F) %>%
      dplyr::select(dx_year.x, time.x, stage.x, smoke.x, S_hat, S_low, S_high, S_obs, S_obs_low, S_obs_high) %>%
      rename(Year = dx_year.x, Time = time.x, Stage = stage.x, Smoke = smoke.x) %>%
      mutate(Stage = sprintf("Stage%s", Stage), Smoke = sprintf("Smoke%s", Smoke))
    output <- bind_rows(output, temp_df)
  }
}

for (row in 1:nrow(output)) {
  if (is.na(output$S_obs[row]) & (output$Time[row] <= 2023 - output$Year[row])) {
    output$S_obs[row] <- output$S_obs[row - 1]
    output$S_obs_low[row] <- output$S_obs_low[row - 1]
    output$S_obs_high[row] <- output$S_obs_high[row - 1]
  } else {
    next
  }
}

write.csv(output, file.path(OUT_DIR, "lung_cancer_survival_estimate_with_ci.csv"), row.names = F)
cat("\nSaved plots (paths; uncomment ggsave to actually write files):\n")
cat(paste0(" - ", plot_paths, collapse = "\n"), "\n")
