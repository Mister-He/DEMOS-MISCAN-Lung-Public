###############################################################################
# Load Parameters and Data for Lung Cancer Simulation
###############################################################################

source("utilis.R")

# ========================================
# 1. MAPPING
# ========================================
mapping <- data.frame(
  race = rep(c("chinese", "malay", "indian", "other"), each = 2),
  gender = rep(c("male", "female"), 4),
  index = 0:7
)

# ========================================
# 2. POPULATION DATA
# ========================================
totpopulation <- fread(
  paste0(params_path, "population/initial_population_params.csv"),
  data.table = FALSE
)

immigrants_historical <- fread(
  paste0(params_path, "population/immigration_historical_params.csv"),
  data.table = FALSE
)

immigrants_projection <- fread(
  paste0(params_path, "population/immigrants.csv"),
  data.table = FALSE
) %>%
  filter(year_immigrated >= 2025)

totpopulation <- rbindlist(
  list(totpopulation, immigrants_historical, immigrants_projection),
  use.names = TRUE,
  fill = TRUE
) %>%
  as.data.frame()

# ========================================
# 3. MORTALITY RATES CUBE
# ========================================
mortality_cube <- read.csv(paste0(params_path, "population/death_params.csv")) %>%
  left_join(mapping, by = c("race", "gender")) %>%
  dplyr::select(age, year_of_death, mortality_rate, index) %>%
  daply("index", function(df) {
    df %>%
      dplyr::select(age, year_of_death, mortality_rate) %>%
      spread(year_of_death, mortality_rate) %>%
      arrange(age) %>%
      dplyr::select(-age) %>%
      .[sort(names(.))] %>%
      data.matrix() %>%
      clean_matrix() %>%
      unname()
  }) %>%
  unname()

# ========================================
# 4. FERTILITY RATES CUBE
# ========================================
fertility_cube <- read.csv(paste0(params_path, "population/fertility_params.csv")) %>%
  dplyr::select(agent_age, sim_year, birth_prob, agent_race) %>%
  mutate(agent_race = factor(agent_race, levels = c("chinese", "malay", "indian", "other"))) %>%
  daply("agent_race", function(df) {
    df %>%
      dplyr::select(agent_age, sim_year, birth_prob) %>%
      spread(sim_year, birth_prob) %>%
      arrange(agent_age) %>%
      dplyr::select(-agent_age) %>%
      .[sort(names(.))] %>%
      data.matrix() %>%
      clean_matrix() %>%
      unname()
  }) %>%
  unname()

# ========================================
# 5. SMOKING PARAMETERS
# ========================================
smoking_params <- read.csv(paste0(params_path, "smoking_params.csv"))[, 2] %>%
  as.matrix()

smoking_calibration_params <- readRDS(
  paste0(params_path, "calibration/smoking_calibration_params.rds")
)

# ========================================
# 6. LUNG CANCER PARAMETERS
# ========================================
lc_baseline_risk_params <- as.matrix(
  read.csv("../params/lc_baseline_risk_params.csv")
)

lc_dwell_weibull_params <- as.matrix(
  read.csv("../params/lc_dwell_weibull_params.csv")
)

lc_transition_rates <- as.matrix(
  read.csv("../params/lc_transition_rates.csv")
)

lc_survival_params <- as.matrix(
  read.csv("../params/lc_survival_params.csv")[, 2]
)

# ========================================
# 7. STRATEGY AND ECONOMIC PARAMETERS
# ========================================
strategy_list <- read.csv(paste0(params_path, "strategy.csv"), header = TRUE)

utility_values <- as.matrix(
  read.csv(paste0(params_path, "utility_values.csv"), header = TRUE)[, -1]
)

ave_cost <- as.matrix(
  read.csv(paste0(params_path, "lc_cost.csv"), header = TRUE)[, -c(1:3)]
)

###############################################################################
# END OF SCRIPT
###############################################################################
