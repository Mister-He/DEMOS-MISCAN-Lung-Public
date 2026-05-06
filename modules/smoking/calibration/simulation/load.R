###############################################################################
# Load Parameters for Smoking Calibration Simulation
###############################################################################

source("utilis.R")
Rcpp::sourceCpp("../core/simulation.cpp")

# ========================================
# 1. POPULATION DATA
# ========================================
totpopulation <- fread(
  paste0(data_path, "population/population.csv"),
  data.table = FALSE
)

immigrants <- fread(
  paste0(data_path, "population/immigrants.csv"),
  data.table = FALSE
)

totpopulation <- rbindlist(
  list(totpopulation, immigrants),
  use.names = TRUE,
  fill = TRUE
) %>%
  as.data.frame() %>%
  sample_n(1e5)

# ========================================
# 2. MORTALITY RATES CUBE
# ========================================
mortality_cube <- read.csv(
  paste0(data_path, "population/mortality_rates_adjusted_with_other.csv")
) %>%
  left_join(mapping, by = c("race", "gender")) %>%
  dplyr::select(year_of_birth, year_of_death, mortality_rate, index) %>%
  daply("index", function(df) {
    df %>%
      dplyr::select(year_of_birth, year_of_death, mortality_rate) %>%
      spread(year_of_death, mortality_rate) %>%
      arrange(year_of_birth) %>%
      dplyr::select(-year_of_birth) %>%
      .[sort(names(.))] %>%
      data.matrix() %>%
      clean_matrix() %>%
      unname()
  }) %>%
  unname()

# ========================================
# 3. FERTILITY RATES
# ========================================
fertility_rates_new <- as.matrix(
  read.csv(paste0(data_path, "population/new_fertility_rates_nonspline.csv"), header = TRUE)
)

###############################################################################
# END OF SCRIPT
###############################################################################
