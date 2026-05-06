# Load necessary libraries
library(stringr)
library(dplyr)

# Initialize output dataframe
output_icer <- data.frame(
  File = character(),
  Strategy = integer(),
  Cost = numeric(),
  QALY = numeric(),
  stringsAsFactors = FALSE
)

# Function to extract cost and QALY per strategy from one file
extract_cost_qaly <- function(file, file_label) {
  lines <- readLines(file)

  # Find all strategy numbers
  strategy_idx <- grep("Strategy [0-9]+", lines)
  result <- data.frame(
    Seed = integer(), Strategy = integer(), Smoking_ban_policy = integer(),
    Cost = numeric(), QALY = numeric(),
    Treatment_cost = numeric(), Screen_cost = numeric(),
    "Life Years" = numeric(), "Total Screens Required" = integer(),
    "Total number of screened people" = integer(), "False Positives" = integer(),
    "Total cases diagnosed" = integer(), "Total Male cases diagnosed" = integer(), "Total Female cases diagnosed" = integer(),
    "Total screened detected" = integer(), "Total Male screened detected" = integer(), "Total Female screened detected" = integer(),
    "Total cases diagnosed<=80" = integer(), "Total Male cases diagnosed<=80" = integer(), "Total Female cases diagnosed<=80" = integer(),
    "Total screened detected<=80" = integer(), "Total Male screened detected<=80" = integer(), "Total Female screened detected<=80" = integer(),
    "Late cases diagnosed" = integer(), "Late Male cases diagnosed" = integer(), "Late Female cases diagnosed" = integer(),
    "Total deaths" = integer(), "Total Male deaths" = integer(), "Total Female deaths" = integer(),
    "Late deaths" = integer(), "Late Male deaths" = integer(), "Late Female deaths" = integer()
  )

  for (i in seq_along(strategy_idx)) {
    idx <- strategy_idx[i]
    seed_num <- as.integer(str_extract_all(lines[idx], "\\d+")[[1]][2])
    strategy_num <- as.integer(str_extract_all(lines[idx], "\\d+")[[1]][3])
    smoking_ban_policy_num <- as.integer(str_extract_all(lines[idx], "\\d+")[[1]][4])

    cost_line <- lines[idx + 4]
    treatment_cost_line <- lines[idx + 5]
    screen_cost_line <- lines[idx + 6]
    qaly_line <- lines[idx + 7]

    life_yrs_line <- lines[idx + 9]
    total_screens_line <- lines[idx + 10]
    total_screened_people_line <- lines[idx + 11]
    false_positives_line <- lines[idx + 12]

    total_case_line <- lines[idx + 14]
    total_screened_detected_line <- lines[idx + 15]
    total_case_l80_line <- lines[idx + 16]
    total_screened_detected_l80_line <- lines[idx + 17]

    late_case_line <- lines[idx + 18]
    total_death_line <- lines[idx + 19]
    late_death_line <- lines[idx + 20]

    cost <- as.numeric(str_replace_all(str_extract(cost_line, "[0-9\\.]+"), ",", ""))
    qaly <- as.numeric(str_replace_all(str_extract(qaly_line, "[0-9\\.]+"), ",", ""))

    treatment_cost <- as.numeric(str_replace_all(str_extract(treatment_cost_line, "[0-9\\.]+"), ",", ""))
    screen_cost <- as.numeric(str_replace_all(str_extract(screen_cost_line, "[0-9\\.]+"), ",", ""))

    life_yrs <- as.numeric(str_replace_all(str_extract(life_yrs_line, "[0-9\\.]+"), ",", ""))
    total_screens <- as.integer(str_extract_all(total_screens_line, "\\d+")[[1]][1])
    total_screened_people <- as.integer(str_extract_all(total_screened_people_line, "\\d+")[[1]][1])
    false_positives <- as.integer(str_extract_all(false_positives_line, "\\d+")[[1]][1])

    total_case <- as.integer(str_extract_all(total_case_line, "\\d+")[[1]][1])
    male_case <- as.integer(str_extract_all(total_case_line, "\\d+")[[1]][2])
    female_case <- as.integer(str_extract_all(total_case_line, "\\d+")[[1]][3])

    total_screen_detected <- as.integer(str_extract_all(total_screened_detected_line, "\\d+")[[1]][1])
    male_screen_detected <- as.integer(str_extract_all(total_screened_detected_line, "\\d+")[[1]][2])
    female_screen_detected <- as.integer(str_extract_all(total_screened_detected_line, "\\d+")[[1]][3])

    total_case_l80 <- as.integer(str_extract_all(total_case_l80_line, "\\d+")[[1]][1])
    male_case_l80 <- as.integer(str_extract_all(total_case_l80_line, "\\d+")[[1]][2])
    female_case_l80 <- as.integer(str_extract_all(total_case_l80_line, "\\d+")[[1]][3])

    total_screen_detected_l80 <- as.integer(str_extract_all(total_screened_detected_l80_line, "\\d+")[[1]][1])
    male_screen_detected_l80 <- as.integer(str_extract_all(total_screened_detected_l80_line, "\\d+")[[1]][2])
    female_screen_detected_l80 <- as.integer(str_extract_all(total_screened_detected_l80_line, "\\d+")[[1]][3])

    late_case <- as.integer(str_extract_all(late_case_line, "\\d+")[[1]][1])
    male_late_case <- as.integer(str_extract_all(late_case_line, "\\d+")[[1]][2])
    female_late_case <- as.integer(str_extract_all(late_case_line, "\\d+")[[1]][3])

    total_death <- as.integer(str_extract_all(total_death_line, "\\d+")[[1]][1])
    male_death <- as.integer(str_extract_all(total_death_line, "\\d+")[[1]][2])
    female_death <- as.integer(str_extract_all(total_death_line, "\\d+")[[1]][3])

    late_death <- as.integer(str_extract_all(late_death_line, "\\d+")[[1]][1])
    male_late_death <- as.integer(str_extract_all(late_death_line, "\\d+")[[1]][2])
    female_late_death <- as.integer(str_extract_all(late_death_line, "\\d+")[[1]][3])

    result <- rbind(result, data.frame(
      Seed = seed_num, Strategy = strategy_num, Smoking_ban_policy = smoking_ban_policy_num,
      Cost = cost, QALY = qaly,
      Treatment_cost = treatment_cost, Screen_cost = screen_cost,
      "Life Years" = life_yrs, "Total Screens Required" = total_screens,
      "Total number of screened people" = total_screened_people, "False Positives" = false_positives,
      "Total cases diagnosed" = total_case, "Total Male cases diagnosed" = male_case, "Total Female cases diagnosed" = female_case,
      "Total screened detected" = total_screen_detected, "Total Male screened detected" = male_screen_detected, "Total Female screened detected" = female_screen_detected,
      "Total cases diagnosed<=80" = total_case_l80, "Total Male cases diagnosed<=80" = male_case_l80, "Total Female cases diagnosed<=80" = female_case_l80,
      "Total screened detected<=80" = total_screen_detected_l80, "Total Male screened detected<=80" = male_screen_detected_l80, "Total Female screened detected<=80" = female_screen_detected_l80,
      "Late cases diagnosed" = late_case, "Late Male cases diagnosed" = male_late_case, "Late Female cases diagnosed" = female_late_case,
      "Total deaths" = total_death, "Total Male deaths" = male_death, "Total Female deaths" = female_death,
      "Late deaths" = late_death, "Late Male deaths" = male_late_death, "Late Female deaths" = female_late_death
    ))
  }

  return(result)
}

seed_grp <- c("1_100")
for (seed_grp_idx in seed_grp) {
  # Loop through each seed
  # Folder containing your files
  file_paths <- list.files(path = paste0("../linux/txts_", seed_grp_idx), pattern = "*.txt", full.names = TRUE)

  # Process each file
  for (file in file_paths) {
    file_label <- basename(file)
    data <- extract_cost_qaly(file, file_label)
    output_icer <- rbind(output_icer, data)
  }

  cat("Completed processing seed group:", seed_grp_idx, "\n")
}

row.names(output_icer) <- NULL

# Order the dataframe by strategy
output_icer <- output_icer %>% arrange(Seed, Smoking_ban_policy, Strategy)

output_icer <- output_icer %>%
  left_join(
    output_icer %>%
      filter(Strategy == 0) %>%
      dplyr::select(Seed, Smoking_ban_policy,
        Base_Cost = Cost,
        Base_QALY = QALY,
        Base_Case = Late.cases.diagnosed,
        Base_Deaths = Total.deaths,
        Base_LY = Life.Years,
        Base_incidence = Total.cases.diagnosed
      ),
    by = c("Seed", "Smoking_ban_policy")
  ) %>%
  rowwise() %>%
  # Compute cost difference only for Strategy ≠ 0
  mutate(
    ICER = (Cost - Base_Cost) / (QALY - Base_QALY),
    QALY_Gain = QALY - Base_QALY,
    Additional_cost = Cost - Base_Cost,
    Deaths_Averted = (Base_Deaths - Total.deaths),
    LYG = (Life.Years - Base_LY),
    OverDiagnosis = (Total.cases.diagnosed - Base_incidence),
    OD_rate = OverDiagnosis / Total.screened.detected * 100,
    FP_rate = False.Positives / Total.Screens.Required * 100,
    LSA = Base_Case - Late.cases.diagnosed
  ) %>%
  dplyr::select(-Base_Cost, -Base_QALY, -Base_Case, -Base_LY, -Base_incidence) %>%
  mutate(Smoking_ban_policy = case_when(
    Smoking_ban_policy == 0 ~ "Baseline",
    Smoking_ban_policy == 1 ~ "Mild (20%)",
    Smoking_ban_policy == 2 ~ "Moderate (50%)",
    Smoking_ban_policy == 3 ~ "High (80%)",
    TRUE ~ "Immediate Ban (100%)"
  ))

strategy_list <- read.csv("../params/strategy_add_MLS10.csv")
bind_rows(replicate(nrow(output_icer) / 33, strategy_list, simplify = F)) %>%
  bind_cols(., output_icer) %>%
  write.csv("../outputs/DEMOS_LC_result/outs_sim/demos_screening_result_with_smoking_ban_policy_table_add_MLS10.csv", row.names = F)


output_icer <- output_icer %>%
  group_by(Strategy, Smoking_ban_policy) %>%
  reframe(
    QALY = median(QALY_Gain, na.rm = TRUE),
    Additional_cost = median(Additional_cost, na.rm = TRUE),
    ICER = Additional_cost / QALY,
    Deaths_Averted = median(Deaths_Averted, na.rm = TRUE),
    LYG = median(LYG, na.rm = TRUE),
    OverDiagnosis = median(OverDiagnosis, na.rm = TRUE),
    LSA = median(LSA, na.rm = TRUE),
    OD_rate = median(OD_rate, na.rm = TRUE),
    FP_rate = median(FP_rate, na.rm = TRUE)
  ) %>%
  mutate(Smoking_ban_policy = factor(Smoking_ban_policy, levels = c("Baseline", "Mild (20%)", "Moderate (50%)", "High (80%)", "Immediate Ban (100%)"))) %>%
  arrange(Strategy, Smoking_ban_policy)

# # View result
View(output_icer)

# ============================================================ #
# Code for deriving information for fhlc table
# ============================================================ #
library(dplyr)

rm(list = ls())
gc()

### TOPSIS base
fhlc <- read.csv("../outputs/DEMOS_LC_result/outs_sim/fhlc/demos_screening_result_with_smoking_ban_policy_table_fhlc_topsis.csv")
fhlc <- fhlc %>% mutate(gender = case_when(
  male_never_smoker_min_age > 0 ~ "male",
  female_never_smoker_min_age > 0 ~ "female",
  T ~ NA
))
fhlc <- fhlc[, -c(59:68)]

main <- read.csv("../outputs/DEMOS_LC_result/outs_sim/aggressive/demos_screening_result_with_smoking_ban_policy_table_with_survival_extrapolation.csv")
main <- main[, -c(59:68)]

male_optimal <- main %>%
  filter(Strategy == 51) %>%
  mutate(Strategy = 0, ID = 0)
female_optimal <- main %>%
  filter(Strategy == 186) %>%
  mutate(Strategy = 0, ID = 0)

# Replacement index in fhlc
idx <- fhlc %>%
  mutate(ID = 1:nrow(.)) %>%
  filter(Strategy == 0) %>%
  pull(ID)

fhlc[idx, ] <- male_optimal %>% mutate(gender = "male") # Put male in
fhlc <- rbind(fhlc, female_optimal %>% mutate(gender = "female")) # Put female in


fhlc <- fhlc %>%
  mutate(
    gender = factor(gender, levels = c("male", "female")),
    Smoking_ban_policy = factor(Smoking_ban_policy, levels = c("Baseline", "Mild (20%)", "Moderate (50%)", "High (80%)", "Immediate Ban (100%)"))
  ) %>%
  arrange(gender, Seed, Smoking_ban_policy, Strategy) %>%
  left_join(
    fhlc %>%
      filter(Strategy == 0) %>%
      dplyr::select(Seed, Smoking_ban_policy, gender,
        Base_Cost = Cost,
        Base_QALY = QALY,
        Base_Case = Late.cases.diagnosed,
        Base_Deaths = Total.deaths,
        Base_LY = Life.Years,
        Base_incidence = Total.cases.diagnosed,
        Base_false_positives = False.Positives,
        Base_screened.detected = Total.screened.detected,
        Base_Screens.Required = Total.Screens.Required
      ),
    by = c("Seed", "Smoking_ban_policy", "gender")
  ) %>%
  rowwise() %>%
  # Compute cost difference only for Strategy ≠ 0
  mutate(
    ICER = (Cost - Base_Cost) / (QALY - Base_QALY),
    QALY_Gain = QALY - Base_QALY,
    Additional_cost = Cost - Base_Cost,
    Deaths_Averted = (Base_Deaths - Total.deaths),
    LYG = (Life.Years - Base_LY),
    OverDiagnosis = (Total.cases.diagnosed - Base_incidence),
    OD_rate = OverDiagnosis / (Total.screened.detected - Base_screened.detected) * 100,
    FP_rate = (False.Positives - Base_false_positives) / (Total.Screens.Required - Base_Screens.Required) * 100,
    LSA = Base_Case - Late.cases.diagnosed
  ) %>%
  dplyr::select(-Base_Cost, -Base_QALY, -Base_Case, -Base_LY, -Base_incidence, -Base_Deaths, -Base_screened.detected, -Base_Screens.Required)

fhlc %>% write.csv("../outputs/DEMOS_LC_result/outs_sim/fhlc/demos_screening_result_with_smoking_ban_policy_table_fhlc_topsis_base.csv")


### MLS10 base


library(dplyr)

rm(list = ls())
gc()

fhlc <- read.csv("../outputs/DEMOS_LC_result/outs_sim/fhlc/demos_screening_result_with_smoking_ban_policy_table_fhlc_MLS10.csv")
fhlc <- fhlc %>% relocate(Strategy, .before = Smoking_ban_policy)
fhlc <- fhlc %>% mutate(gender = case_when(
  male_never_smoker_min_age > 0 ~ "male",
  female_never_smoker_min_age > 0 ~ "female",
  T ~ NA
))
fhlc <- fhlc[, -c(59:60, 63:68)]

main <- read.csv("../outputs/DEMOS_LC_result/outs_sim/aggressive/demos_screening_result_with_smoking_ban_policy_table_with_survival_extrapolation.csv")
main <- main[, -c(59:60, 63:68)]

male_optimal <- main %>%
  filter(Strategy == 22) %>%
  mutate(Strategy = 0, ID = 0)
female_optimal <- main %>%
  filter(Strategy == 157) %>%
  mutate(Strategy = 0, ID = 0)

# Replacement index in fhlc
idx <- fhlc %>%
  mutate(ID = 1:nrow(.)) %>%
  filter(Strategy == 0) %>%
  pull(ID)

fhlc[idx, ] <- male_optimal %>% mutate(gender = "male") # Put male in
fhlc <- rbind(fhlc, female_optimal %>% mutate(gender = "female")) # Put female in


fhlc <- fhlc %>%
  mutate(
    gender = factor(gender, levels = c("male", "female")),
    Smoking_ban_policy = factor(Smoking_ban_policy, levels = c("Baseline", "Mild (20%)", "Moderate (50%)", "High (80%)", "Immediate Ban (100%)"))
  ) %>%
  arrange(gender, Seed, Smoking_ban_policy, Strategy) %>%
  left_join(
    fhlc %>%
      filter(Strategy == 0) %>%
      dplyr::select(Seed, Smoking_ban_policy, gender,
        Base_Cost = Cost,
        Base_Additional_cost = Additional_cost,
        Base_QALY = QALY,
        Base_QALY_Gain = QALY_Gain,
        Base_Case = Late.cases.diagnosed,
        Base_Deaths = Total.deaths,
        Base_LY = Life.Years,
        Base_incidence = Total.cases.diagnosed,
        Base_false_positives = False.Positives,
        Base_screened.detected = Total.screened.detected,
        Base_Screens.Required = Total.Screens.Required
      ),
    by = c("Seed", "Smoking_ban_policy", "gender")
  ) %>%
  rowwise() %>%
  # Compute cost difference only for Strategy ≠ 0
  mutate(
    ICER = (Cost - Base_Cost) / (QALY - Base_QALY),
    QALY_Gain = QALY - Base_QALY,
    Additional_cost = Cost - Base_Cost,
    Deaths_Averted = (Base_Deaths - Total.deaths),
    LYG = (Life.Years - Base_LY),
    OverDiagnosis = (Total.cases.diagnosed - Base_incidence),
    OD_rate = OverDiagnosis / (Total.screened.detected - Base_screened.detected) * 100,
    FP_rate = (False.Positives - Base_false_positives) / (Total.Screens.Required - Base_Screens.Required) * 100,
    LSA = Base_Case - Late.cases.diagnosed
  ) %>%
  dplyr::select(-Base_Cost, -Base_QALY, -Base_Case, -Base_LY, -Base_incidence, -Base_Deaths, -Base_screened.detected, -Base_Screens.Required)

fhlc %>% write.csv("../outputs/DEMOS_LC_result/outs_sim/fhlc/demos_screening_result_with_smoking_ban_policy_table_fhlc_MLS10_base.csv")
