###############################################################################
# Data Preprocessing for Smoking History Generator
# Combines NHS, MEC, and SP2 data into transition format
###############################################################################
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(purrr)

# Clear the environment
rm(list = ls())
gc()

# ==============================================================================#
# Data Loading
# ==============================================================================#
# Load National Health Survey, MEC and SP2 data (require data dictionary)
data_nhs <- readRDS("data/data_nhs.rds")
data_mec <- readRDS("data/data_mec.rds")
data_sp2 <- readRDS("data/data_sp2.rds")
data_3 <- readRDS("data/data_3.rds")

# Remove repeated IDs in each dataset
data_nhs <- data_nhs %>% distinct(id_sp2, .keep_all = T)
data_mec <- data_mec %>% distinct(id_mec1, .keep_all = T)
data_sp2 <- data_sp2 %>% distinct(id_sp2, .keep_all = T)
data_3 <- data_3 %>% distinct(id_sp2, .keep_all = T)

# Check all there duplicate IDs in NHS, SP2 and data3
# Seems there are no overlap in those IDs in four datasets
cat("ID in NHS and ID in data3 have", length(intersect(data_nhs$id_sp2, data_3$id_sp2)), "in common\n")
cat("ID in NHS and ID in SP2 have", length(intersect(data_nhs$id_sp2, data_sp2$id_sp2)), "in common\n")
cat("ID in SP2 and ID in NHS have", length(intersect(data_nhs$id_sp2, data_sp2$id_sp2)), "in common\n")

# ==============================================================================#
# NHS data
# ==============================================================================#
data_nhs_processed <- data_nhs %>%
  # 1) Compute absolute start/stop dates and indicator flags
  mutate(
    date_start = yob + smoke_astart,
    date_stop = yob + smoke_astop,
    smoke_start = ifelse(is.na(smoke_astart), NA, 1),
    smoke_stop = ifelse(is.na(smoke_astop), NA, 2)
  ) %>%
  rename(date1 = date) %>%
  select(-smoke_astart, -smoke_astop) %>%
  # 2) Pivot the 7 columns (date, date_start, date_stop,
  #                    smoke1, smoke2, smoke_start, smoke_stop)
  #    into long form of (metric, which, val)
  pivot_longer(
    cols = c(
      date1, date_start, date_stop,
      smoke1, smoke_start, smoke_stop
    ),
    names_to = c("metric", "which"),
    names_pattern = "(date|smoke)_?(1|2|start|end|stop)",
    values_to = "val"
  ) %>%
  # 3) Separate metric into two columns: date and smoke
  pivot_wider(
    names_from  = metric,
    values_from = val
  ) %>%
  # 4) Combine and sort
  mutate(age = date - yob) %>%
  filter(age >= 12) %>%
  select(id_sp2, gender, race, yob, which, date, smoke, intensity) %>%
  arrange(id_sp2, date) %>%
  # 5) Keep only rows where both date and smoke are present
  filter(!is.na(date) & !is.na(smoke)) %>%
  arrange(id_sp2, date) %>%
  # 6) Assign a run‐ID (“wave”) for each contiguous block of identical smoke status,
  #    then collapse each run to its earliest date
  group_by(id_sp2) %>%
  mutate(wave = rleid(which)) %>%
  group_by(id_sp2, wave) %>%
  reframe(
    gender = first(gender),
    race = first(race),
    yob = first(yob),
    year = min(date),
    state = first(smoke),
    intensity = max(intensity),
    .groups = "drop"
  ) %>%
  arrange(id_sp2, year) %>%
  # 7) Compute the next time‐point and state; force any 1→0 or 2→0 transitions to become →2
  group_by(id_sp2) %>%
  mutate(
    next_year = lead(year),
    next_state = lead(state),
    next_state = case_when(
      state %in% c(1, 2) & next_state == 0 ~ 2,
      TRUE ~ next_state
    ),
    next_year = case_when(
      year == next_year ~ next_year + 1,
      TRUE ~ next_year
    ),
    # “Expected” next state at the previous row, then update the current state if needed
    expectation_state = lag(next_state),
    expectation_year = lag(next_year),
    state = replace(state, state == 0 & expectation_state == 2, 2),
    year = if_else(
      !is.na(expectation_year) & year != expectation_year,
      expectation_year,
      year
    )
  ) %>%
  ungroup() %>%
  # 7) Keep rows where the state actually changes (or where there is no next state)
  filter(!is.na(next_state)) %>%
  # # 8) Recompute next_year/next_state after that adjustment
  # group_by(id_sp2) %>%
  # mutate(
  #   next_year  = lead(year),
  #   next_state = lead(state),
  #   next_state = case_when(
  #     state %in% c(1,2) & next_state == 0 ~ 2,
  #     TRUE                              ~ next_state
  #   )
  # ) %>%
  # ungroup() %>%

  # 9) Sort and drop “outcome” rows that have wave > 1 but no following year
  arrange(id_sp2, gender, race, yob, year) %>%
  filter(!is.na(next_year)) %>%
  # 10) Rename into the final shape
  transmute(
    id      = id_sp2,
    gender,
    race,
    yob,
    intensity,
    year.a  = year,
    state.a = state,
    year.h  = next_year,
    state.h = next_state
  )

# ==============================================================================#
# SP2 data
# ==============================================================================#
data_sp2_processed <- data_sp2 %>%
  # 1) Compute absolute start/stop dates and indicator flags
  mutate(
    date_start = yob + smoke_astart,
    date_stop = yob + smoke_astop,
    smoke_start = ifelse(is.na(smoke_astart), NA, 1),
    smoke_stop = ifelse(is.na(smoke_astop), NA, 2)
  ) %>%
  select(-smoke_astart, -smoke_astop) %>%
  # 2) Pivot the 8 columns (date1, date2, date_start, date_stop,
  #                    smoke1, smoke2, smoke_start, smoke_stop)
  #    into long form of (metric, which, val)
  pivot_longer(
    cols = c(
      date1, date2, date_start, date_stop,
      smoke1, smoke2, smoke_start, smoke_stop
    ),
    names_to = c("metric", "which"),
    names_pattern = "(date|smoke)_?(1|2|start|end|stop)",
    values_to = "val"
  ) %>%
  # 3) Separate metric into two columns: date and smoke
  pivot_wider(
    names_from  = metric,
    values_from = val
  )

# 1. Compute artificial age-12 row
age12_rows <- data_sp2_processed %>%
  distinct(id_sp2, gender, race, yob) %>%
  mutate(
    date = yob + 12,
    smoke = 0,
    intensity = 0,
    which = "init"
  )

# 2. Keep only those who do *not* already have a real record at or before age 12
age12_rows <- age12_rows %>%
  anti_join(
    data_sp2_processed %>%
      mutate(age = date - yob) %>%
      filter(age <= 12),
    by = "id_sp2"
  )

# 3. Combine and sort
data_sp2_processed <- data_sp2_processed %>%
  select(id_sp2, gender, race, yob, which, date, smoke, intensity) %>%
  bind_rows(age12_rows %>% select(id_sp2, gender, race, yob, which, date, smoke, intensity)) %>%
  arrange(id_sp2, date)

data_sp2_processed <- data_sp2_processed %>%
  # 4) Keep only rows where both date and smoke are present
  filter(!is.na(date) & !is.na(smoke)) %>%
  arrange(id_sp2, date) %>%
  # 5) Assign a run‐ID (“wave”) for each contiguous block of identical smoke status,
  #    then collapse each run to its earliest date
  group_by(id_sp2) %>%
  mutate(wave = rleid(which)) %>%
  group_by(id_sp2, wave) %>%
  reframe(
    gender = first(gender),
    race = first(race),
    yob = first(yob),
    year = min(date),
    state = first(smoke),
    intensity = max(intensity),
    .groups = "drop"
  ) %>%
  arrange(id_sp2, year) %>%
  # 6) Compute the next time‐point and state; force any 1→0 or 2→0 transitions to become →2
  group_by(id_sp2) %>%
  mutate(
    next_year = lead(year),
    next_state = lead(state),
    next_state = case_when(
      state %in% c(1, 2) & next_state == 0 ~ 2,
      TRUE ~ next_state
    ),
    next_year = case_when(
      year == next_year ~ next_year + 1,
      TRUE ~ next_year
    ),
    # “Expected” next state at the previous row, then update the current state if needed
    expectation_state = lag(next_state),
    expectation_year = lag(next_year),
    state = replace(state, state == 0 & expectation_state == 2, 2),
    year = if_else(
      !is.na(expectation_year) & year != expectation_year,
      expectation_year,
      year
    )
  ) %>%
  ungroup() %>%
  # 7) Keep rows where the state actually changes (or where there is no next state)
  filter(!is.na(next_state)) %>%
  # # 8) Recompute next_year/next_state after that adjustment
  # group_by(id_sp2) %>%
  # mutate(
  #   next_year  = lead(year),
  #   next_state = lead(state),
  #   next_state = case_when(
  #     state %in% c(1,2) & next_state == 0 ~ 2,
  #     TRUE                              ~ next_state
  #   )
  # ) %>%
  # ungroup() %>%

  # 9) Sort and drop “outcome” rows that have wave > 1 but no following year
  arrange(id_sp2, gender, race, yob, year) %>%
  filter(!is.na(next_year)) %>%
  # 10) Rename into the final shape
  transmute(
    id      = id_sp2,
    gender,
    race,
    yob,
    intensity,
    year.a  = year,
    state.a = state,
    year.h  = next_year,
    state.h = next_state
  )

# ==============================================================================#
# MEC data
# ==============================================================================#
data_mec_processed <- data_mec %>%
  # 1) Compute absolute start/stop dates and indicator flags
  mutate(
    date_start = yob + smoke_astart,
    date_stop = yob + smoke_astop,
    smoke_start = ifelse(is.na(smoke_astart), NA, 1),
    smoke_stop = ifelse(is.na(smoke_astop), NA, 2)
  ) %>%
  select(-smoke_astart, -smoke_astop, -id_mec2) %>%
  # 2) Pivot the 8 columns (date1, date2, date_start, date_stop,
  #                    smoke1, smoke2, smoke_start, smoke_stop)
  #    into long form of (metric, which, val)
  pivot_longer(
    cols = c(
      date1, date2, date_start, date_stop,
      smoke1, smoke2, smoke_start, smoke_stop
    ),
    names_to = c("metric", "which"),
    names_pattern = "(date|smoke)_?(1|2|start|end|stop)",
    values_to = "val"
  ) %>%
  # 3) Separate metric into two columns: date and smoke
  pivot_wider(
    names_from  = metric,
    values_from = val
  )

# 1. Compute artificial age-12 row
age12_rows <- data_mec_processed %>%
  distinct(id_mec1, gender, race, yob) %>%
  mutate(
    date = yob + 12,
    smoke = 0,
    intensity = 0,
    which = "init"
  )

# 2. Keep only those who do *not* already have a real record at or before age 12
age12_rows <- age12_rows %>%
  anti_join(
    data_mec_processed %>%
      mutate(age = date - yob) %>%
      filter(age <= 12),
    by = "id_mec1"
  )

# 3. Combine and sort
data_mec_processed <- data_mec_processed %>%
  select(id_mec1, gender, race, yob, which, date, smoke, intensity) %>%
  bind_rows(age12_rows %>% select(id_mec1, gender, race, yob, which, date, smoke, intensity)) %>%
  arrange(id_mec1, date)

data_mec_processed <- data_mec_processed %>%
  # 4) Keep only rows where both date and smoke are present
  filter(!is.na(date) & !is.na(smoke)) %>%
  arrange(id_mec1, date) %>%
  # 5) Assign a run‐ID (“wave”) for each contiguous block of identical smoke status,
  #    then collapse each run to its earliest date
  group_by(id_mec1) %>%
  mutate(wave = rleid(which)) %>%
  group_by(id_mec1, wave) %>%
  reframe(
    gender = first(gender),
    race = first(race),
    yob = first(yob),
    year = min(date),
    state = first(smoke),
    intensity = max(intensity),
    .groups = "drop"
  ) %>%
  arrange(id_mec1, year) %>%
  # 6) Compute the next time‐point and state; force any 1→0 or 2→0 transitions to become →2
  group_by(id_mec1) %>%
  mutate(
    next_year = lead(year),
    next_state = lead(state),
    next_state = case_when(
      state %in% c(1, 2) & next_state == 0 ~ 2,
      TRUE ~ next_state
    ),
    next_year = case_when(
      year == next_year ~ next_year + 1,
      TRUE ~ next_year
    ),
    # “Expected” next state at the previous row, then update the current state if needed
    expectation_state = lag(next_state),
    expectation_year = lag(next_year),
    state = replace(state, state == 0 & expectation_state == 2, 2),
    year = if_else(
      !is.na(expectation_year) & year != expectation_year,
      expectation_year,
      year
    )
  ) %>%
  ungroup() %>%
  # 7) Keep rows where the state actually changes (or where there is no next state)
  filter(!is.na(next_state)) %>%
  # # 8) Recompute next_year/next_state after that adjustment
  # group_by(id_mec1) %>%
  # mutate(
  #   next_year  = lead(year),
  #   next_state = lead(state),
  #   next_state = case_when(
  #     state %in% c(1,2) & next_state == 0 ~ 2,
  #     TRUE                              ~ next_state
  #   )
  # ) %>%
  # ungroup() %>%

  # 9) Sort and drop “outcome” rows that have wave > 1 but no following year
  arrange(id_mec1, gender, race, yob, year) %>%
  filter(!is.na(next_year)) %>%
  # 10) Rename into the final shape
  transmute(
    id      = id_mec1,
    gender,
    race,
    yob,
    intensity,
    year.a  = year,
    state.a = state,
    year.h  = next_year,
    state.h = next_state
  )

# ==============================================================================#
# Data 3
# ==============================================================================#
data_3_processed <- data_3 %>%
  distinct(id_sp2, .keep_all = T) %>%
  # 1) Compute absolute start/stop dates and indicator flags
  mutate(
    date_start = yob + smoke_astart,
    date_stop = yob + smoke_astop,
    smoke_start = ifelse(is.na(smoke_astart), NA, 1),
    smoke_stop = ifelse(is.na(smoke_astop), NA, 2)
  ) %>%
  select(-smoke_astart, -smoke_astop, -id_revisit) %>%
  # 2) Pivot the 8 columns (date1, date2, date_start, date_stop,
  #                    smoke1, smoke2, smoke_start, smoke_stop)
  #    into long form of (metric, which, val)
  pivot_longer(
    cols = c(
      date1, date2, date3, date_start, date_stop,
      smoke1, smoke2, smoke3, smoke_start, smoke_stop
    ),
    names_to = c("metric", "which"),
    names_pattern = "(date|smoke)_?(1|2|3|start|end|stop)",
    values_to = "val"
  ) %>%
  # 3) Separate metric into two columns: date and smoke
  pivot_wider(
    names_from  = metric,
    values_from = val
  )

# 1. Compute artificial age-12 row
age12_rows <- data_3_processed %>%
  distinct(id_sp2, gender, race, yob) %>%
  mutate(
    date = yob + 12,
    smoke = 0,
    intensity = 0,
    which = "init"
  )

# 2. Keep only those who do *not* already have a real record at or before age 12
age12_rows <- age12_rows %>%
  anti_join(
    data_3_processed %>%
      mutate(age = date - yob) %>%
      filter(age <= 12),
    by = "id_sp2"
  )

# 3. Combine and sort
data_3_processed <- data_3_processed %>%
  select(id_sp2, gender, race, yob, which, date, smoke, intensity) %>%
  bind_rows(age12_rows %>% select(id_sp2, gender, race, yob, which, date, smoke, intensity)) %>%
  arrange(id_sp2, date)

data_3_processed <- data_3_processed %>%
  # 4) Keep only rows where both date and smoke are present
  filter(!is.na(date) & !is.na(smoke)) %>%
  arrange(id_sp2, date) %>%
  # 5) Assign a run‐ID (“wave”) for each contiguous block of identical smoke status,
  #    then collapse each run to its earliest date
  group_by(id_sp2) %>%
  mutate(wave = rleid(which)) %>%
  group_by(id_sp2, wave) %>%
  reframe(
    gender = first(gender),
    race = first(race),
    yob = first(yob),
    year = min(date),
    state = first(smoke),
    intensity = max(intensity),
    .groups = "drop"
  ) %>%
  arrange(id_sp2, year) %>%
  # 6) Compute the next time‐point and state; force any 1→0 or 2→0 transitions to become →2
  group_by(id_sp2) %>%
  mutate(
    next_year = lead(year),
    next_state = lead(state),
    next_state = case_when(
      state %in% c(1, 2) & next_state == 0 ~ 2,
      TRUE ~ next_state
    ),
    next_year = case_when(
      year == next_year ~ next_year + 1,
      TRUE ~ next_year
    ),
    # “Expected” next state at the previous row, then update the current state if needed
    expectation_state = lag(next_state),
    expectation_year = lag(next_year),
    state = replace(state, state == 0 & expectation_state == 2, 2),
    year = if_else(
      !is.na(expectation_year) & year != expectation_year,
      expectation_year,
      year
    )
  ) %>%
  ungroup() %>%
  # Do it again

  group_by(id_sp2) %>%
  mutate(
    next_year = lead(year),
    next_state = lead(state),
    next_state = case_when(
      state %in% c(1, 2) & next_state == 0 ~ 2,
      TRUE ~ next_state
    ),
    next_year = case_when(
      year == next_year ~ next_year + 1,
      TRUE ~ next_year
    ),
    # “Expected” next state at the previous row, then update the current state if needed
    expectation_state = lag(next_state),
    expectation_year = lag(next_year),
    state = replace(state, state == 0 & expectation_state == 2, 2),
    year = if_else(
      !is.na(expectation_year) & year != expectation_year,
      expectation_year,
      year
    )
  ) %>%
  ungroup() %>%
  # 7) Keep rows where the state actually changes (or where there is no next state)
  filter(!is.na(next_state)) %>%
  # # 8) Recompute next_year/next_state after that adjustment
  # group_by(id_sp2) %>%
  # mutate(
  #   next_year  = lead(year),
  #   next_state = lead(state),
  #   next_state = case_when(
  #     state %in% c(1,2) & next_state == 0 ~ 2,
  #     TRUE                              ~ next_state
  #   )
  # ) %>%
  # ungroup() %>%

  # 9) Sort and drop “outcome” rows that have wave > 1 but no following year
  arrange(id_sp2, gender, race, yob, year) %>%
  filter(!is.na(next_year)) %>%
  # 10) Rename into the final shape
  transmute(
    id      = id_sp2,
    gender,
    race,
    yob,
    intensity,
    year.a  = year,
    state.a = state,
    year.h  = next_year,
    state.h = next_state
  )

# ==============================================================================#
# Combine all datasets
# ==============================================================================#
# Combine all processed data sets into one
data_combined <- bind_rows(
  data_nhs_processed,
  data_sp2_processed,
  data_mec_processed,
  data_3_processed
) %>%
  filter(!is.na(gender)) %>%
  # # Replace those state.a==2,state.h==1 to state.a==1
  # mutate(
  #   state.a = if_else(state.a == 2L & state.h == 1L, 1L, state.a)
  # ) %>%
  arrange(id, year.a)


data_combined <- data_combined %>%
  mutate(age.a = year.a - yob, age.h = year.h - yob)


msm_data <- data_combined %>%
  mutate(state = state.a, age = age.a) %>%
  select(id, age, state, gender, race, yob, intensity) %>%
  # Bind with second row: state.j at age.j
  bind_rows(
    data_combined %>%
      mutate(state = state.h, age = age.h) %>%
      select(id, age, state, gender, race, yob, intensity)
  ) %>%
  distinct(id, age, state, .keep_all = T) %>%
  arrange(id, age)


saveRDS(data_combined, "data/transition_data_collapsed.rds")
saveRDS(msm_data, "data/data_combined.rds")

# ========================================
# 5. GENERATE PLOTS
# ========================================
source("smoking_data_plot_functions.R")

d01 <- data_combined %>%
  filter(state.a == 0) %>%
  mutate(event = (state.h == 1) * 1)

fit01 <- survfit(Surv(age.a, age.h, event) ~ gender, data = d01)

d12 <- data_combined %>%
  filter(state.a == 1) %>%
  mutate(event = (state.h == 2) * 1)

fit12 <- survfit(Surv(age.a, age.h, event) ~ gender, data = d12)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))

pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
plot_km_survival(fit01, subplot_label = "a)")
popViewport()

pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
plot_km_survival(fit12, subplot_label = "b)")
popViewport()

popViewport()

ecdf_male <- compute_ecdf(msm_data, 1, "Male")
ecdf_female <- compute_ecdf(msm_data, 2, "Female")
ecdf_combined <- rbind(ecdf_male, ecdf_female)

summary_levels <- c(10, 20, 30, 40, 50, 60)
ecdf_summary <- ecdf_combined %>%
  filter(cigarettes_per_day %in% summary_levels) %>%
  mutate(cumulative_pct = paste0(round(cumulative_prob * 100, 2), "%")) %>%
  select(gender, cigarettes_per_day, cumulative_prob, cumulative_pct)

print(ecdf_summary)

grid.newpage()
plot_ecdf_intensity(ecdf_combined)

cat("\n=== ECDF Summary Statistics ===\n\n")

for (g in c("Male", "Female")) {
  cat(sprintf("Gender: %s\n", g))

  gender_data <- ecdf_combined %>% filter(gender == g)
  percentiles <- c(0.25, 0.50, 0.75, 0.90, 0.95)

  for (p in percentiles) {
    cigs <- gender_data %>%
      filter(cumulative_prob >= p) %>%
      slice(1) %>%
      pull(cigarettes_per_day)

    cat(sprintf(
      "  %d%% of smokers consume ≤ %d cigarettes/day\n",
      round(p * 100), cigs
    ))
  }
  cat("\n")
}

###############################################################################
# END OF SCRIPT
###############################################################################
