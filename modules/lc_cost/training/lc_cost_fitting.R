###############################################################################
# Lung Cancer Treatment Cost Modeling
# GAM with Gamma family and log link
###############################################################################

library(mgcv)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(patchwork)

# ========================================
# 1. DATA PREPARATION
# ========================================
df <- read_csv("data/Lung2009-2019_table4_20241217.csv")

df <- df %>%
  mutate(
    n = ifelse(n == "less than 5", 4, n),
    mean = ifelse(mean == 0, 1, mean)
  ) %>%
  mutate(
    smoking_grp = as.factor(smoking_grp),
    stage = as.factor(stage),
    G134_bigrouping = as.factor(G134_bigrouping),
    n = as.numeric(n)
  ) %>%
  group_by(smoking_grp, stage, YEAR) %>%
  reframe(
    n = sum(n),
    total_year_cost = sum(total_year_cost)
  ) %>%
  mutate(mean = total_year_cost / n)

# Singapore medical Consumer Price Index table from 2009-2019, set 2024 to be 100
# Ref: https://tablebuilder.singstat.gov.sg/table/TS/M213801
cpi_table <- data.frame(
  Year = 1:10,
  CPI = c(
    72.752, 74.092, 75.868, 79.281, 82.374,
    84.722, 84.575, 85.46, 87.628, 89.436
  )
)

df <- df %>%
  left_join(cpi_table, by = c("YEAR" = "Year")) %>%
  mutate(mean = mean * (100 / CPI)) %>%
  dplyr::select(-CPI)

# ========================================
# 3. FIT GAM MODEL
# ========================================
gam_model_gamma <- gam(
  mean ~ smoking_grp + stage +
    s(YEAR, by = interaction(smoking_grp, stage), k = 4),
  data = df,
  family = Gamma(link = "log"),
  weights = n,
  method = "REML"
)

summary(gam_model_gamma)

# ========================================
# 4. MODEL DIAGNOSTICS
# ========================================
df$resid_dev <- residuals(gam_model_gamma, type = "deviance")
df$fitted <- fitted(gam_model_gamma)
df$linear_pred <- predict(gam_model_gamma, type = "link")

p1 <- ggplot(df, aes(sample = resid_dev)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  ggtitle("QQ Plot of Deviance Residuals") +
  theme_minimal()

p2 <- ggplot(df, aes(x = linear_pred, y = resid_dev)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Linear Predictor", y = "Deviance Residuals") +
  ggtitle("Residuals vs. Linear Predictor") +
  theme_minimal()

p3 <- ggplot(df, aes(x = resid_dev)) +
  geom_histogram(binwidth = 50, fill = "gray70", color = "black") +
  labs(x = "Residuals", y = "Frequency") +
  ggtitle("Histogram of Residuals") +
  theme_minimal()

p4 <- ggplot(df, aes(x = fitted, y = mean)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", size = 1) +
  labs(x = "Fitted Values", y = "Observed Mean Cost") +
  ggtitle("Observed vs. Predicted Cost") +
  theme_minimal()

(p1 + p2) / (p3 + p4)

# ========================================
# 5. PREDICTIONS
# ========================================
newdata <- expand.grid(
  smoking_grp = c("non-smokers", "smokers"),
  stage = c("Stage I", "Stage II", "Stage III", "Stage IV"),
  n = 1,
  YEAR = 1:20
)

pred_resp <- predict(gam_model_gamma,
  newdata = newdata,
  type = "response", se.fit = TRUE
)

plot_data <- newdata %>%
  mutate(
    predicted_cost = pred_resp$fit,
    lower_CI = pmax(pred_resp$fit - 1.96 * pred_resp$se.fit, 0),
    upper_CI = pred_resp$fit + 1.96 * pred_resp$se.fit,
    se = pred_resp$se.fit
  ) %>%
  arrange(smoking_grp, stage, YEAR)

# ========================================
# 6. VISUALIZATION
# ========================================
df_obs <- df %>%
  transmute(YEAR, smoking_grp, stage, obs_cost = mean) %>%
  mutate(across(c(smoking_grp, stage), as.factor))

lvl_stage <- levels(plot_data$stage)
lvl_smoke <- levels(plot_data$smoking_grp)

plot_data <- plot_data %>%
  mutate(
    stage = factor(stage, levels = lvl_stage),
    smoking_grp = factor(smoking_grp, levels = lvl_smoke)
  )

df_obs <- df_obs %>%
  mutate(
    stage = factor(stage, levels = lvl_stage),
    smoking_grp = factor(smoking_grp, levels = lvl_smoke)
  )

pal_stage <- c(
  "Stage I" = "#E64B35", "Stage II" = "#4DBBD5",
  "Stage III" = "#00A087", "Stage IV" = "#7E6148"
)

ggplot() +
  geom_ribbon(
    data = plot_data,
    aes(
      x = YEAR, ymin = lower_CI, ymax = upper_CI, fill = stage,
      group = interaction(stage, smoking_grp)
    ),
    alpha = 0.22, colour = NA
  ) +
  geom_line(
    data = plot_data,
    aes(
      x = YEAR, y = predicted_cost, color = stage,
      group = interaction(stage, smoking_grp)
    ),
    linewidth = 0.9
  ) +
  geom_point(
    data = df_obs,
    aes(x = YEAR, y = obs_cost, color = stage),
    size = 2.1, alpha = 0.85, position = position_jitter(width = 0.05, height = 0)
  ) +
  facet_grid(smoking_grp ~ stage) +
  scale_color_manual(values = pal_stage, name = "Stage") +
  scale_fill_manual(values = pal_stage, name = "Stage") +
  labs(
    title = "Predicted vs Observed Treatment Cost",
    x = "Years Since Diagnosis",
    y = "Cost (SGD)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  )

# ========================================
# 7. EXPORT RESULTS
# ========================================
plot_data %>%
  dplyr::select(YEAR, smoking_grp, stage, predicted_cost, se) %>%
  write.csv("../../../params/lc_cost.csv", row.names = FALSE)

###############################################################################
# END OF SCRIPT
###############################################################################
