library(dplyr)

# Acquire all column names
all_columns <- c(
  "male_ever_smoker_min_age", "male_ever_smoker_max_age", "male_ever_smoker_uptake",
  "male_ever_smoker_pack_yrs", "male_ever_smoker_quitting_period", "male_ever_smoker_frequency", "male_ever_smoker_family_history",
  "male_never_smoker_min_age", "male_never_smoker_max_age", "male_never_smoker_uptake",
  "male_never_smoker_frequency", "male_never_smoker_family_history",
  "female_ever_smoker_min_age", "female_ever_smoker_max_age", "female_ever_smoker_uptake",
  "female_ever_smoker_pack_yrs", "female_ever_smoker_quitting_period", "female_ever_smoker_frequency", "female_ever_smoker_family_history",
  "female_never_smoker_min_age", "female_never_smoker_max_age", "female_never_smoker_uptake",
  "female_never_smoker_frequency", "female_never_smoker_family_history"
)

# ==============================================================================#
# Design all other strategies except for two suggested ones
# ==============================================================================#
# Male / Female Smoker
## Start age: 50, 55, 60, 65
## End age: 75, 80, 85
## Coverage: 0.1:1.0 every 0.1
## Pack-years: 15, 20, 30
## Quitting period: 15, 20, 30
## Frequency: 1, 2

### For Male
male_ever_smoker <- expand.grid(
  male_ever_smoker_min_age = 50, # seq(50, 60, 5),
  male_ever_smoker_max_age = 80, # c(75, 80, 85),
  male_ever_smoker_uptake = 1.0, # seq(0.4, 1.0, 0.3),
  male_ever_smoker_pack_yrs = 20,
  male_ever_smoker_quitting_period = 15,
  male_ever_smoker_frequency = 1,
  male_ever_smoker_family_history = 1
)

### For Female
female_ever_smoker <- expand.grid(
  female_ever_smoker_min_age = 50, # seq(50, 60, 5),
  female_ever_smoker_max_age = 80, # c(75, 80, 85),
  female_ever_smoker_uptake = 1.0, # seq(0.4, 1.0, 0.3),
  female_ever_smoker_pack_yrs = 20,
  female_ever_smoker_quitting_period = 15,
  female_ever_smoker_frequency = 1, # 1:5,
  female_ever_smoker_family_history = 1
)

# Male / Female Never Smoker
## Start age: 50, 55, 60, 65
## End age: 75, 80, 85
## Uptake: 0.01, 0.05, 0.1, 0.5, 1.0
## Frequency: 1, 2, 5

### For Male
male_never_smoker <- expand.grid(
  male_never_smoker_min_age = seq(50, 60, 5),
  male_never_smoker_max_age = c(75, 80, 85),
  male_never_smoker_uptake = seq(0.4, 1.0, 0.3),
  male_never_smoker_frequency = 1:5,
  male_never_smoker_family_history = 1
)

### For Female
female_never_smoker <- expand.grid(
  female_never_smoker_min_age = seq(50, 60, 5),
  female_never_smoker_max_age = c(75, 80, 85),
  female_never_smoker_uptake = seq(0.4, 1.0, 0.3),
  female_never_smoker_frequency = 1:5,
  female_never_smoker_family_history = 1
)

# ==============================================================================#
# Aggregate all strategies
# ==============================================================================#
# Add all possible columns and fill in the missing columns with zero values
fill_missing <- function(df, all_cols) {
  missing_cols <- setdiff(all_cols, colnames(df))
  df[missing_cols] <- 0
  df %>% dplyr::select(all_of(all_cols))
}

# Fill in the missing columns
male_never_smoker <- fill_missing(bind_cols(male_ever_smoker %>% mutate(male_ever_smoker_family_history = 1), male_never_smoker), all_columns)
female_never_smoker <- fill_missing(bind_cols(female_ever_smoker %>% mutate(female_ever_smoker_family_history = 1), female_never_smoker), all_columns)
male_ever_smoker <- fill_missing(male_ever_smoker, all_columns)
female_ever_smoker <- fill_missing(female_ever_smoker, all_columns)

# Column bind all strategies
all_strategies <- bind_rows(
  male_ever_smoker,
  female_ever_smoker
) %>%
  na.omit() %>%
  tibble::add_column(screen = 1, .before = 1) %>%
  tibble::add_column(ID = 1:nrow(.), .before = 1) %>%
  tibble::add_row(ID = 0, screen = 0, .before = 1) %>%
  mutate(across(everything(), ~ replace(., is.na(.) & ID == 0, 0)))

never_smoker_strategies <- bind_rows(
  male_never_smoker,
  female_never_smoker
) %>%
  na.omit() %>%
  tibble::add_column(screen = 1, .before = 1) %>%
  tibble::add_column(ID = 1:nrow(.), .before = 1) %>%
  tibble::add_row(ID = 0, screen = 0, .before = 1) %>%
  mutate(across(everything(), ~ replace(., is.na(.) & ID == 0, 0)))

# Save into the strategy data file
write.csv(all_strategies, "../params/strategy_add_MLS10.csv", row.names = FALSE)
write.csv(never_smoker_strategies, "../params/never_smoker_strategy_ls_od_control.csv", row.names = FALSE)
