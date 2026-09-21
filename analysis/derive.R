library(dplyr)
# ============================================================ #
# Some exploratory analysis to understand the simulation results better
# ============================================================ #
# Number of cases diagnosed cases by smoke status, gender, year diagnosed and age in 2009-2019
populationdata %>%
  mutate(
    smoke_status = case_when(
      year_start_smoking == 0 ~ "Never Smoker",
      year_start_smoking > 0 ~ "Ever Smoker",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(year_lung_cancer_diagnosed %in% 2009:2019) %>%
  group_by(gender, smoke_status, year_lung_cancer_diagnosed, stage_diagnosed) %>%
  reframe(case = n()) %>%
  pivot_wider(names_from = year_lung_cancer_diagnosed, values_from = case, values_fill = 0)

# Number of cases diagnosed cases by smoke status (former smoker), gender in 2009-2019
populationdata %>%
  filter(year_lung_cancer_diagnosed %in% (2009:2019)) %>%
  mutate(
    smoke_status = case_when(
      year_start_smoking == 0 ~ "Never Smoker",
      year_lung_cancer > year_stop_smoking & year_stop_smoking > 0 ~ "Former Smoker",
      TRUE ~ "Current Smoker"
    )
  ) %>%
  group_by(gender, smoke_status) %>%
  reframe(case = n()) %>%
  pivot_wider(names_from = gender, values_from = case, values_fill = 0)

# Number of cases diagnosed cases by smoke status (defined by duration), gender in 2009-2019
populationdata %>%
  filter(year_lung_cancer_diagnosed > 0) %>%
  mutate(duration = ifelse(smoking_intensity > 0,
    ifelse(year_stop_smoking > 0,
      year_stop_smoking - year_start_smoking,
      year_lung_cancer_diagnosed - year_start_smoking
    ), 0
  )) %>%
  mutate(
    smoke_status = case_when(
      duration <= 5 ~ "Never Smoker",
      duration > 5 ~ "Ever Smoker",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(year_lung_cancer_diagnosed %in% 2009:2019) %>%
  group_by(gender, smoke_status) %>%
  reframe(case = n()) %>%
  pivot_wider(names_from = gender, values_from = case, values_fill = 0)

# Mean/median duration of smoking by gender for cohort born after 1990
populationdata %>%
  filter(year_start_smoking > 0, year_of_birth > 1990) %>%
  mutate(duration = ifelse(year_stop_smoking > 0, year_stop_smoking - year_start_smoking, 90)) %>%
  group_by(gender) %>%
  reframe(
    mean_duration = mean(duration, na.rm = TRUE),
    median_duration = median(duration, na.rm = TRUE)
  )

# Mean/median duration of age when diagnosed with LC in entire population
populationdata %>%
  filter(year_lung_cancer_diagnosed > 0) %>%
  mutate(age_onset = year_lung_cancer - year_of_birth) %>%
  group_by(gender) %>%
  reframe(
    mean_age = mean(age_onset, na.rm = TRUE),
    median_age = median(age_onset, na.rm = TRUE)
  )

# Mean/median life duration after diagnosed with LC in entire population
populationdata %>%
  filter(Lung_specific > 0) %>%
  mutate(age_death = year_of_death - year_lung_cancer_diagnosed) %>%
  group_by(gender, stage_diagnosed) %>%
  reframe(
    mean_age = mean(age_death, na.rm = TRUE),
    median_age = median(age_death, na.rm = TRUE)
  )

# Age distribution of initiating smoking
populationdata %>%
  filter(year_start_smoking > 0, gender == "male", year_of_birth > 1990) %>%
  mutate(age_start = year_start_smoking - year_of_birth) %>%
  pull(age_start) %>%
  table() %>%
  plot()

# ============================================================ #
# Table S6
# ============================================================ #
library(dplyr)
library(writexl)
output_icer <- read.csv("outputs/DEMOS_LC_result/outs_sim/aggressive/demos_screening_result_with_smoking_ban_policy_table_with_survival_extrapolation.csv")
output_icer <- output_icer %>%
  mutate(gender = case_when(
    male_ever_smoker_min_age > 0 ~ "male",
    female_ever_smoker_min_age > 0 ~ "female",
    T ~ "baseline"
  )) %>%
  mutate(grp = paste0(gender, "_", Smoking_ban_policy))
output_icer %>%
  filter(gender != "baseline") %>%
  group_by(grp) %>%
  reframe(
    QALY = paste0(round(median(QALY) / 1e6, 2), " (", round(quantile(QALY, 0.025) / 1e6, 2), "-", round(quantile(QALY, 0.975) / 1e6, 2), ")"),
    Late_cases_diagnosed = paste0(round(median(Late.cases.diagnosed) / 1e3, 2), " (", round(quantile(Late.cases.diagnosed, 0.025) / 1e3, 2), "-", round(quantile(Late.cases.diagnosed, 0.975) / 1e3, 2), ")"),
    Total_deaths = paste0(round(median(Total.deaths) / 1e3, 2), " (", round(quantile(Total.deaths, 0.025) / 1e3, 2), "-", round(quantile(Total.deaths, 0.975) / 1e3, 2), ")"),
    Total_costs = paste0(round(median(Cost) / 1e9, 2), " (", round(quantile(Cost, 0.025) / 1e9, 2), "-", round(quantile(Cost, 0.975) / 1e9, 2), ")")
  ) %>%
  mutate(grp = factor(grp, levels = c(
    "male_Baseline", "male_Mild (20%)", "male_Moderate (50%)", "male_High (80%)", "male_Immediate Ban (100%)",
    "female_Baseline", "female_Mild (20%)", "female_Moderate (50%)", "female_High (80%)", "female_Immediate Ban (100%)"
  ))) %>%
  arrange(grp) %>%
  write_xlsx("temp.xlsx")

# ============================================================ #
# Table S7
# ============================================================ #
library(writexl)
output_icer %>%
  filter(gender != "baseline") %>%
  arrange(Strategy, Smoking_ban_policy) %>%
  filter(Strategy %in% c(22, 157)) %>%
  group_by(grp) %>%
  reframe(
    QG = paste0(round(median(QALY_Gain) / 1e3, 2), " (", round(quantile(QALY_Gain, 0.025) / 1e3, 2), "–", round(quantile(QALY_Gain, 0.975) / 1e3, 2), ")"),
    LSA = paste0(round(median(LSA) / 1e3, 2), " (", round(quantile(LSA, 0.025) / 1e3, 2), "–", round(quantile(LSA, 0.975) / 1e3, 2), ")"),
    DA = paste0(round(median(Deaths_Averted) / 1e3, 2), " (", round(quantile(Deaths_Averted, 0.025) / 1e3, 2), "–", round(quantile(Deaths_Averted, 0.975) / 1e3, 2), ")"),
    AddCost = paste0(round(median(Additional_cost) / 1e9, 2), " (", round(quantile(Additional_cost, 0.025) / 1e9, 2), "–", round(quantile(Additional_cost, 0.975) / 1e9, 2), ")"),
    OD = paste0(round(median(OD_rate), 2), " (", round(quantile(OD_rate, 0.025), 2), "–", round(quantile(OD_rate, 0.975), 2), ")"),
    FP = paste0(round(median(FP_rate), 2), " (", round(quantile(FP_rate, 0.025), 2), "–", round(quantile(FP_rate, 0.975), 2), ")"),
    ICER = paste0(round(median(ICER) / 1e3, 2), " (", round(quantile(ICER, 0.025) / 1e3, 2), "–", round(quantile(ICER, 0.975) / 1e3, 2), ")")
  ) %>%
  mutate(grp = factor(grp, levels = c(
    "male_Baseline", "male_Mild (20%)", "male_Moderate (50%)", "male_High (80%)", "male_Immediate Ban (100%)",
    "female_Baseline", "female_Mild (20%)", "female_Moderate (50%)", "female_High (80%)", "female_Immediate Ban (100%)"
  ))) %>%
  arrange(grp) %>%
  write_xlsx("temp.xlsx")

# ============================================================ #
# Table S8
# ============================================================ #
output_icer %>%
  filter(gender != "baseline") %>%
  arrange(Strategy, Smoking_ban_policy) %>%
  filter(Strategy %in% c(51, 186)) %>%
  group_by(grp) %>%
  reframe(
    QG = paste0(round(median(QALY_Gain) / 1e3, 2), " (", round(quantile(QALY_Gain, 0.025) / 1e3, 2), "–", round(quantile(QALY_Gain, 0.975) / 1e3, 2), ")"),
    LSA = paste0(round(median(LSA) / 1e3, 2), " (", round(quantile(LSA, 0.025) / 1e3, 2), "–", round(quantile(LSA, 0.975) / 1e3, 2), ")"),
    DA = paste0(round(median(Deaths_Averted) / 1e3, 2), " (", round(quantile(Deaths_Averted, 0.025) / 1e3, 2), "–", round(quantile(Deaths_Averted, 0.975) / 1e3, 2), ")"),
    AddCost = paste0(round(median(Additional_cost) / 1e9, 2), " (", round(quantile(Additional_cost, 0.025) / 1e9, 2), "–", round(quantile(Additional_cost, 0.975) / 1e9, 2), ")"),
    OD = paste0(round(median(OD_rate), 2), " (", round(quantile(OD_rate, 0.025), 2), "–", round(quantile(OD_rate, 0.975), 2), ")"),
    FP = paste0(round(median(FP_rate), 2), " (", round(quantile(FP_rate, 0.025), 2), "–", round(quantile(FP_rate, 0.975), 2), ")"),
    ICER = paste0(round(median(ICER) / 1e3, 2), " (", round(quantile(ICER, 0.025) / 1e3, 2), "–", round(quantile(ICER, 0.975) / 1e3, 2), ")")
  ) %>%
  mutate(grp = factor(grp, levels = c(
    "male_Baseline", "male_Mild (20%)", "male_Moderate (50%)", "male_High (80%)", "male_Immediate Ban (100%)",
    "female_Baseline", "female_Mild (20%)", "female_Moderate (50%)", "female_High (80%)", "female_Immediate Ban (100%)"
  ))) %>%
  arrange(grp) %>%
  write_xlsx("temp.xlsx")

# ============================================================ #
# Numbers in `Status quo with lung cancer screening`
# ============================================================ #
## MLSOD10 ICER under status quo tobacco control program
library(dplyr)
library(tidyr)

output_icer = read.csv("../outputs/DEMOS_LC_result/outs_sim/aggressive/demos_screening_result_with_smoking_ban_policy_table_with_survival_extrapolation.csv")

# Statuo quo with lung cancer screening
# MLSOD10 under status uo vs. no screening under status quo
output_icer %>%
  filter(
    Strategy %in% c(22),
    Smoking_ban_policy == "Baseline"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(157),
        Smoking_ban_policy == "Baseline"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain,
        Base_LSA = LSA,
        Base_DA = Deaths_Averted,
        Base_AddCost = Additional_cost,
        Base_Cost = Cost,
        Base_OD = OverDiagnosis,
        Base_total_screened_detected = Total.screened.detected,
        Base_FP = False.Positives,
        Base_total_screenes_required = Total.Screens.Required
      )
  ) %>%
  rowwise() %>%
  mutate(
    QG = QALY_Gain + Base_QG,
    LSA = LSA + Base_LSA,
    DA = Deaths_Averted + Base_DA,
    AddCost = Additional_cost + Base_AddCost,
    OD = 100 * (OverDiagnosis + Base_OD) / (Total.screened.detected + Base_total_screened_detected),
    FP = 100 * (False.Positives + Base_FP) / (Total.Screens.Required + Base_total_screenes_required),
    ICER = AddCost / QG
  ) %>%
  group_by(Smoking_ban_policy) %>%
  reframe(
    QG = paste0(round(quantile(QG, 0.5), 2), " (95% UI: ", round(quantile(QG, 0.025), 2), "–", round(quantile(QG, 0.975), 2), ")"),
    LSA = paste0(round(quantile(LSA, 0.5), 2), " (95% UI: ", round(quantile(LSA, 0.025), 2), "–", round(quantile(LSA, 0.975), 2), ")"),
    DA = paste0(round(quantile(DA, 0.5), 2), " (95% UI: ", round(quantile(DA, 0.025), 2), "–", round(quantile(DA, 0.975), 2), ")"),
    Cost = paste0(round(quantile(Cost, 0.5), 2), " (95% UI: ", round(quantile(Cost, 0.025), 2), "–", round(quantile(Cost, 0.975), 2), ")"),
    AddCost = paste0(round(quantile(AddCost, 0.5), 2), " (95% UI: ", round(quantile(AddCost, 0.025), 2), "–", round(quantile(AddCost, 0.975), 2), ")"),
    OD = paste0(round(quantile(OD, 0.5), 2), " (95% UI: ", round(quantile(OD, 0.025), 2), "–", round(quantile(OD, 0.975), 2), ")"),
    FP = paste0(round(quantile(FP, 0.5), 2), " (95% UI: ", round(quantile(FP, 0.025), 2), "–", round(quantile(FP, 0.975), 2), ")"),
    ICER = paste0(round(quantile(ICER, 0.5), 2), " (95% UI: ", round(quantile(ICER, 0.025), 2), "–", round(quantile(ICER, 0.975), 2), ")")
  ) %>% 
  t() %>% 
print

# TRS under status uo vs. no screening under status quo
output_icer %>%
  filter(
    Strategy %in% c(51),
    Smoking_ban_policy == "Baseline"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(186),
        Smoking_ban_policy == "Baseline"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain,
        Base_LSA = LSA,
        Base_DA = Deaths_Averted,
        Base_AddCost = Additional_cost,
        Base_Cost = Cost,
        Base_OD = OverDiagnosis,
        Base_total_screened_detected = Total.screened.detected,
        Base_FP = False.Positives,
        Base_total_screenes_required = Total.Screens.Required
      )
  ) %>%
  rowwise() %>%
  mutate(
    QG = QALY_Gain + Base_QG,
    LSA = LSA + Base_LSA,
    DA = Deaths_Averted + Base_DA,
    AddCost = Additional_cost + Base_AddCost,
    OD = 100 * (OverDiagnosis + Base_OD) / (Total.screened.detected + Base_total_screened_detected),
    FP = 100 * (False.Positives + Base_FP) / (Total.Screens.Required + Base_total_screenes_required),
    ICER = AddCost / QG
  ) %>%
  group_by(Smoking_ban_policy) %>%
  reframe(
    QG = paste0(round(quantile(QG, 0.5), 2), " (95% UI: ", round(quantile(QG, 0.025), 2), "–", round(quantile(QG, 0.975), 2), ")"),
    LSA = paste0(round(quantile(LSA, 0.5), 2), " (95% UI: ", round(quantile(LSA, 0.025), 2), "–", round(quantile(LSA, 0.975), 2), ")"),
    DA = paste0(round(quantile(DA, 0.5), 2), " (95% UI: ", round(quantile(DA, 0.025), 2), "–", round(quantile(DA, 0.975), 2), ")"),
    Cost = paste0(round(quantile(Cost, 0.5), 2), " (95% UI: ", round(quantile(Cost, 0.025), 2), "–", round(quantile(Cost, 0.975), 2), ")"),
    AddCost = paste0(round(quantile(AddCost, 0.5), 2), " (95% UI: ", round(quantile(AddCost, 0.025), 2), "–", round(quantile(AddCost, 0.975), 2), ")"),
    OD = paste0(round(quantile(OD, 0.5), 2), " (95% UI: ", round(quantile(OD, 0.025), 2), "–", round(quantile(OD, 0.975), 2), ")"),
    FP = paste0(round(quantile(FP, 0.5), 2), " (95% UI: ", round(quantile(FP, 0.025), 2), "–", round(quantile(FP, 0.975), 2), ")"),
    ICER = paste0(round(quantile(ICER, 0.5), 2), " (95% UI: ", round(quantile(ICER, 0.025), 2), "–", round(quantile(ICER, 0.975), 2), ")")
  ) %>% 
  t() %>% 
print

######## Use MLSOD10 under status quo as baseline ########
# MLSOD10 as reference
ref = output_icer %>%
  filter(
    Strategy %in% c(22),
    Smoking_ban_policy == "Baseline"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(157),
        Smoking_ban_policy == "Baseline"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain,
        Base_LSA = LSA,
        Base_DA = Deaths_Averted,
        Base_AddCost = Additional_cost,
        Base_Cost = Cost,
        Base_OD = OverDiagnosis,
        Base_total_screened_detected = Total.screened.detected,
        Base_FP = False.Positives,
        Base_total_screenes_required = Total.Screens.Required
      )
  ) %>%
  rowwise() %>%
  mutate(
    QG = QALY_Gain + Base_QG,
    LSA = LSA + Base_LSA,
    DA = Deaths_Averted + Base_DA,
    AddCost = Additional_cost + Base_AddCost,
    OD = 100 * (OverDiagnosis + Base_OD) / (Total.screened.detected + Base_total_screened_detected),
    FP = 100 * (False.Positives + Base_FP) / (Total.Screens.Required + Base_total_screenes_required),
    ICER = AddCost / QG
  )

# MLSOD10 vs 55-80, other settings same under status quo
compare = output_icer %>%
  filter(
    Strategy %in% c(23),
    Smoking_ban_policy == "Baseline"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(158),
        Smoking_ban_policy == "Baseline"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain,
        Base_LSA = LSA,
        Base_DA = Deaths_Averted,
        Base_AddCost = Additional_cost,
        Base_Cost = Cost,
        Base_OD = OverDiagnosis,
        Base_total_screened_detected = Total.screened.detected,
        Base_FP = False.Positives,
        Base_total_screenes_required = Total.Screens.Required
      )
  ) %>%
  rowwise() %>%
  mutate(
    QG = QALY_Gain + Base_QG,
    LSA = LSA + Base_LSA,
    DA = Deaths_Averted + Base_DA,
    AddCost = Additional_cost + Base_AddCost,
    OD = 100 * (OverDiagnosis + Base_OD) / (Total.screened.detected + Base_total_screened_detected),
    FP = 100 * (False.Positives + Base_FP) / (Total.Screens.Required + Base_total_screenes_required),
    ICER = AddCost / QG
  )
round(quantile((ref$QG - compare$QG)/ref$QG * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$AddCost - compare$AddCost)/ref$AddCost * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$ICER - compare$ICER)/ref$ICER * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$OD - compare$OD)/ref$OD * 100, c(0.025, 0.5, 0.975)), 1)

# MLSOD10 vs 50-75, other settings same under status quo
compare = output_icer %>%
  filter(
    Strategy %in% c(19),
    Smoking_ban_policy == "Baseline"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(154),
        Smoking_ban_policy == "Baseline"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain,
        Base_LSA = LSA,
        Base_DA = Deaths_Averted,
        Base_AddCost = Additional_cost,
        Base_Cost = Cost,
        Base_OD = OverDiagnosis,
        Base_total_screened_detected = Total.screened.detected,
        Base_FP = False.Positives,
        Base_total_screenes_required = Total.Screens.Required
      )
  ) %>%
  rowwise() %>%
  mutate(
    QG = QALY_Gain + Base_QG,
    LSA = LSA + Base_LSA,
    DA = Deaths_Averted + Base_DA,
    AddCost = Additional_cost + Base_AddCost,
    OD = 100 * (OverDiagnosis + Base_OD) / (Total.screened.detected + Base_total_screened_detected),
    FP = 100 * (False.Positives + Base_FP) / (Total.Screens.Required + Base_total_screenes_required),
    ICER = AddCost / QG
  )
round(quantile((ref$QG - compare$QG)/ref$QG * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$AddCost - compare$AddCost)/ref$AddCost * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$ICER - compare$ICER)/ref$ICER * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$OD - compare$OD)/ref$OD * 100, c(0.025, 0.5, 0.975)), 1)

# MLSOD10 vs 40% uptake, other settings same under status quo
compare = output_icer %>%
  filter(
    Strategy %in% c(4),
    Smoking_ban_policy == "Baseline"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(139),
        Smoking_ban_policy == "Baseline"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain,
        Base_LSA = LSA,
        Base_DA = Deaths_Averted,
        Base_AddCost = Additional_cost,
        Base_Cost = Cost,
        Base_OD = OverDiagnosis,
        Base_total_screened_detected = Total.screened.detected,
        Base_FP = False.Positives,
        Base_total_screenes_required = Total.Screens.Required
      )
  ) %>%
  rowwise() %>%
  mutate(
    QG = QALY_Gain + Base_QG,
    LSA = LSA + Base_LSA,
    DA = Deaths_Averted + Base_DA,
    AddCost = Additional_cost + Base_AddCost,
    OD = 100 * (OverDiagnosis + Base_OD) / (Total.screened.detected + Base_total_screened_detected),
    FP = 100 * (False.Positives + Base_FP) / (Total.Screens.Required + Base_total_screenes_required),
    ICER = AddCost / QG
  )
round(quantile((ref$QG - compare$QG)/ref$QG * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$AddCost - compare$AddCost)/ref$AddCost * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$ICER - compare$ICER)/ref$ICER * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$OD - compare$OD)/ref$OD * 100, c(0.025, 0.5, 0.975)), 1)

# MLSOD10 vs quinquennial screening, other settings same under status quo
compare = output_icer %>%
  filter(
    Strategy %in% c(130),
    Smoking_ban_policy == "Baseline"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(265),
        Smoking_ban_policy == "Baseline"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain,
        Base_LSA = LSA,
        Base_DA = Deaths_Averted,
        Base_AddCost = Additional_cost,
        Base_Cost = Cost,
        Base_OD = OverDiagnosis,
        Base_total_screened_detected = Total.screened.detected,
        Base_FP = False.Positives,
        Base_total_screenes_required = Total.Screens.Required
      )
  ) %>%
  rowwise() %>%
  mutate(
    QG = QALY_Gain + Base_QG,
    LSA = LSA + Base_LSA,
    DA = Deaths_Averted + Base_DA,
    AddCost = Additional_cost + Base_AddCost,
    OD = 100 * (OverDiagnosis + Base_OD) / (Total.screened.detected + Base_total_screened_detected),
    FP = 100 * (False.Positives + Base_FP) / (Total.Screens.Required + Base_total_screenes_required),
    ICER = AddCost / QG
  )
round(quantile((ref$QG - compare$QG)/ref$QG * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$AddCost - compare$AddCost)/ref$AddCost * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$ICER - compare$ICER)/ref$ICER * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$OD - compare$OD)/ref$OD * 100, c(0.025, 0.5, 0.975)), 1)


# MLSOD10 vs 50-85, other settings same under status quo
compare = output_icer %>%
  filter(
    Strategy %in% c(25),
    Smoking_ban_policy == "Baseline"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(160),
        Smoking_ban_policy == "Baseline"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain,
        Base_LSA = LSA,
        Base_DA = Deaths_Averted,
        Base_AddCost = Additional_cost,
        Base_Cost = Cost,
        Base_OD = OverDiagnosis,
        Base_total_screened_detected = Total.screened.detected,
        Base_FP = False.Positives,
        Base_total_screenes_required = Total.Screens.Required
      )
  ) %>%
  rowwise() %>%
  mutate(
    QG = QALY_Gain + Base_QG,
    LSA = LSA + Base_LSA,
    DA = Deaths_Averted + Base_DA,
    AddCost = Additional_cost + Base_AddCost,
    OD = 100 * (OverDiagnosis + Base_OD) / (Total.screened.detected + Base_total_screened_detected),
    FP = 100 * (False.Positives + Base_FP) / (Total.Screens.Required + Base_total_screenes_required),
    ICER = AddCost / QG
  )
round(quantile((ref$QG - compare$QG)/ref$QG * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$AddCost - compare$AddCost)/ref$AddCost * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$ICER - compare$ICER)/ref$ICER * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$OD - compare$OD)/ref$OD * 100, c(0.025, 0.5, 0.975)), 1)

######## Smoking ban with lung cancer screening ######## 
# MLSOD10 under immediate ban vs. MLSOD10 under status quo
# MLSOD10 under status quo as reference
ref = output_icer %>%
  filter(
    Strategy %in% c(22),
    Smoking_ban_policy == "Baseline"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(157),
        Smoking_ban_policy == "Baseline"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain,
        Base_LSA = LSA,
        Base_DA = Deaths_Averted,
        Base_AddCost = Additional_cost,
        Base_Cost = Cost,
        Base_OD = OverDiagnosis,
        Base_total_screened_detected = Total.screened.detected,
        Base_FP = False.Positives,
        Base_total_screenes_required = Total.Screens.Required
      )
  ) %>%
  rowwise() %>%
  mutate(
    QALY = QALY + Base_QG,
    LS = Late.cases.diagnosed - Base_LSA,
    Deaths = Total.deaths - Base_DA,
    Cost = Cost + Base_AddCost,
    OverDiagnosis = OverDiagnosis + Base_OD,
    FalsePositives = False.Positives + Base_FP
  )
round(quantile((ref$QALY), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Cost), c(0.025, 0.5, 0.975)), 1)

# Comapre with MLSOD10 under immediate ban
compare = output_icer %>%
  filter(
    Strategy %in% c(22),
    Smoking_ban_policy == "Immediate Ban (100%)"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(157),
        Smoking_ban_policy == "Immediate Ban (100%)"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain,
        Base_LSA = LSA,
        Base_DA = Deaths_Averted,
        Base_AddCost = Additional_cost,
        Base_Cost = Cost,
        Base_OD = OverDiagnosis,
        Base_total_screened_detected = Total.screened.detected,
        Base_FP = False.Positives,
        Base_total_screenes_required = Total.Screens.Required
      )
  ) %>%
  rowwise() %>%
  mutate(
    QALY = QALY + Base_QG,
    LS = Late.cases.diagnosed - Base_LSA,
    Deaths = Total.deaths - Base_DA,
    Cost = Cost + Base_AddCost,
    OverDiagnosis = OverDiagnosis + Base_OD,
    FalsePositives = False.Positives + Base_FP,
    ICER = (Additional_cost) / QALY_Gain
  )
round(quantile((ref$QALY - compare$QALY), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$QALY - compare$QALY) / ref$QALY_Gain, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Cost - compare$Cost), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Cost - compare$Cost) / ref$Cost * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$LS - compare$LS), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Deaths - compare$Deaths), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Deaths - compare$Deaths) / ref$Deaths_Averted, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$OverDiagnosis - compare$OverDiagnosis), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$FalsePositives - compare$FalsePositives), c(0.025, 0.5, 0.975)), 1)

# TRS under immediate ban vs. TRS under status quo
# TRS under status quo as reference
ref = output_icer %>%
  filter(
    Strategy %in% c(51),
    Smoking_ban_policy == "Baseline"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(186),
        Smoking_ban_policy == "Baseline"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain,
        Base_LSA = LSA,
        Base_DA = Deaths_Averted,
        Base_AddCost = Additional_cost,
        Base_Cost = Cost,
        Base_OD = OverDiagnosis,
        Base_total_screened_detected = Total.screened.detected,
        Base_FP = False.Positives,
        Base_total_screenes_required = Total.Screens.Required
      )
  ) %>%
  rowwise() %>%
  mutate(
    QALY = QALY + Base_QG,
    LS = Late.cases.diagnosed - Base_LSA,
    Deaths = Total.deaths - Base_DA,
    Cost = Cost + Base_AddCost,
    OverDiagnosis = OverDiagnosis + Base_OD,
    FalsePositives = False.Positives + Base_FP
  )

# Comapre with TRS under immediate ban
compare = output_icer %>%
  filter(
    Strategy %in% c(51),
    Smoking_ban_policy == "Immediate Ban (100%)"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(186),
        Smoking_ban_policy == "Immediate Ban (100%)"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain,
        Base_LSA = LSA,
        Base_DA = Deaths_Averted,
        Base_AddCost = Additional_cost,
        Base_Cost = Cost,
        Base_OD = OverDiagnosis,
        Base_total_screened_detected = Total.screened.detected,
        Base_FP = False.Positives,
        Base_total_screenes_required = Total.Screens.Required
      )
  ) %>%
  rowwise() %>%
  mutate(
    QALY = QALY + Base_QG,
    LS = Late.cases.diagnosed - Base_LSA,
    Deaths = Total.deaths - Base_DA,
    Cost = Cost + Base_AddCost,
    OverDiagnosis = OverDiagnosis + Base_OD,
    FalsePositives = False.Positives + Base_FP
  )
round(quantile((ref$QALY - compare$QALY), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$QALY - compare$QALY) / ref$QALY_Gain, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Cost - compare$Cost), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Cost - compare$Cost) / ref$Cost * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$LS - compare$LS), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Deaths - compare$Deaths), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Deaths - compare$Deaths) / ref$Deaths_Averted, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$OverDiagnosis - compare$OverDiagnosis), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$FalsePositives - compare$FalsePositives), c(0.025, 0.5, 0.975)), 1)

####### Dicussion #######
# MLSOD10/TRS under immediate ban vs. MLSOD10/TRS under status quo
# MLSOD10/TRS under status quo as reference
ref = output_icer %>%
  filter(
    Strategy %in% c(0),
    Smoking_ban_policy == "Baseline"
  ) %>% 
  dplyr::select(
    Seed,
    Cost = Cost,
    Deaths = Total.deaths
  )

# Compare with MLDOS10/TRS under immediate ban
compare = output_icer %>%
  filter(
    Strategy %in% c(22,51),
    Smoking_ban_policy == "Immediate Ban (100%)"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(157,186),
        Smoking_ban_policy == "Immediate Ban (100%)"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain,
        Base_LSA = LSA,
        Base_DA = Deaths_Averted,
        Base_AddCost = Additional_cost,
        Base_Cost = Cost,
        Base_OD = OverDiagnosis,
        Base_total_screened_detected = Total.screened.detected,
        Base_FP = False.Positives,
        Base_total_screenes_required = Total.Screens.Required
      )
  ) %>%
  rowwise() %>%
  mutate(
    QALY = QALY + Base_QG,
    LS = Late.cases.diagnosed - Base_LSA,
    Deaths = Total.deaths - Base_DA,
    Cost = Cost + Base_AddCost,
    OverDiagnosis = OverDiagnosis + Base_OD,
    FalsePositives = False.Positives + Base_FP
  ) %>% 
  dplyr::select(
    Seed, Cost, Deaths
  ) %>% 
  left_join(
    ref,
    by = 'Seed',
    suffix = c('','_baseline')
  )
round(quantile(100 * (1 - compare$Cost / compare$Cost_baseline), c(0.025, 0.5, 0.975)), 1)
round(quantile(100 * (1 - compare$Deaths / compare$Deaths_baseline), c(0.025, 0.5, 0.975)), 1)

# For all strategies, the ICER under immediate ban vs. status quo
# All strategies under baseline as reference
ref <- output_icer %>%
  filter(
    Strategy %in% c(1:135),
    Smoking_ban_policy == "Baseline"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(136:270),
        Smoking_ban_policy == "Baseline"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain,
        Base_AddCost = Additional_cost
      )
  ) %>%
  rowwise() %>%
  mutate(
    QG = QALY_Gain + Base_QG,
    AddCost = Additional_cost + Base_AddCost,
    ICER = AddCost / QG
  )

# Compare with all strategies under immediate ban
compare <- output_icer %>%
  filter(
    Strategy %in% c(1:135),
    Smoking_ban_policy == "Immediate Ban (100%)"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(136:270),
        Smoking_ban_policy == "Immediate Ban (100%)"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain,
        Base_AddCost = Additional_cost
      )
  ) %>%
  rowwise() %>%
  mutate(
    QALY_Gain = QALY_Gain + Base_QG,
    Additional_cost = Additional_cost + Base_AddCost,
    ICER = (Additional_cost) / QALY_Gain
  )
round(quantile((ref$ICER), c(0.025, 0.5, 0.975)), 1)
round(quantile((compare$ICER), c(0.025, 0.5, 0.975)), 1)
round(quantile((compare$ICER - ref$ICER), c(0.025, 0.5, 0.975)), 1)

# # Light smokers (dont't require merging by sexes, but just in case)
# output_icer = read.csv("../outputs/DEMOS_LC_result/outs_sim/aggressive/demos_screening_result_with_smoking_ban_policy_table_with_survival_extrapolation.csv")
# output_icer_ls = read.csv('../outputs/DEMOS_LC_result/outs_sim/packyear/demos_screening_result_with_smoking_ban_policy_table_add_MLS10.csv')
# ref = output_icer_ls %>% 
#   filter(
#     Strategy %in% c(1:16)
#   ) %>%
#   dplyr::select(
#     Seed, Strategy, Smoking_ban_policy, QALY, Cost
#   ) %>% 
#   cbind(
#     .,
#     output_icer_ls %>% 
#       filter(
#         Strategy %in% c(17:32)
#       ) %>% 
#       dplyr::select(
#         female_QG = QALY_Gain,
#         female_AddCost = Additional_cost
#       )
#   ) %>%
#   left_join(
#     output_icer %>% 
#       filter(
#         Strategy %in% c(0)
#       ) %>% 
#       dplyr::select(
#         Seed, Smoking_ban_policy, Base_QALY = QALY, Base_Cost = Cost
#       ),
#     by = c("Seed", "Smoking_ban_policy")
#   ) %>% 
#   rowwise() %>%
#   mutate(
#     QALY = QALY + female_QG,
#     Cost = Cost + female_AddCost,
#     QALY_Gain = QALY - Base_QALY,
#     Additional_cost = Cost - Base_Cost,
#     ICER = (Additional_cost) / QALY_Gain
#   )
# round(quantile(pull(filter(ref, Strategy == 9, Smoking_ban_policy == "Immediate Ban (100%)"), ICER), c(0.025, 0.5, 0.975)), 1)

###### FHLC #######
# MLSOD10 to ever-smokers + individuals with family history under immediate ban
output_icer = read.csv("../outputs/DEMOS_LC_result/outs_sim/aggressive/demos_screening_result_with_smoking_ban_policy_table_with_survival_extrapolation.csv")
output_icer_fhlc = read.csv('../outputs/DEMOS_LC_result/outs_sim/fhlc/demos_screening_result_with_smoking_ban_policy_table_fhlc_MLS10_base.csv') # Gender-specific

# In FHLC, MLSOD10 also contains those ineligible ever-smokers' QALY Gain and Addcost, need to add them
# MLSOD10 over ever-smokers + family history screening generated for subsequent analysis
ref = output_icer_fhlc %>%
   filter(
     gender == 'male'
   ) %>%
   cbind(
     .,
     output_icer_fhlc %>%
       filter(
         gender == 'female'
       ) %>%
       dplyr::select(
         Base_QG = QALY_Gain,
         Base_AddCost = Additional_cost
       )
   ) %>%
   rowwise() %>%
   mutate(
     QALY_Gain = QALY_Gain + Base_QG,
     Additional_cost = Additional_cost + Base_AddCost,
     QALY = QALY + Base_QG,
     Cost = Cost + Base_AddCost,
     ICER = (Additional_cost) / QALY_Gain
   )

round(quantile(pull(filter(ref, Strategy != 0, Smoking_ban_policy == "Immediate Ban (100%)"), QALY_Gain), c(0.025, 0.5, 0.975)), 1)
round(quantile(pull(filter(ref, Strategy != 0, Smoking_ban_policy == "Immediate Ban (100%)"), Additional_cost) / 1e6, c(0.025, 0.5, 0.975)), 1)

# Calculate combined program under immediate ban vs. no screening under immediate ban
compare = ref %>% 
  filter(
    Strategy != 0,
    Smoking_ban_policy == "Immediate Ban (100%)"
  ) %>% 
  dplyr::select(
    Seed, QALY, Cost
  ) %>% 
  left_join(
    output_icer %>% 
      filter(
        Strategy == 0,
        Smoking_ban_policy == "Immediate Ban (100%)"
      ) %>% 
      dplyr::select(
        Seed, QALY, Cost
      ),
    by = "Seed",
    suffix = c("_ban", "_baseline")
  ) %>% 
  mutate(
    QALY_Gain = QALY_ban - QALY_baseline,
    Additional_cost = Cost_ban - Cost_baseline,
    ICER = Additional_cost / QALY_Gain
  )
round(quantile(compare$ICER, c(0.025, 0.5, 0.975)), 1)

