################################## Analysis code for manuscript and SI ##################################
## MLSOD10 ICER under status quo tobacco control program
library(dplyr)
library(tidyr)

output_icer = read.csv("../outputs/DEMOS_LC_result/outs_sim/aggressive/demos_screening_result_with_smoking_ban_policy_table_with_survival_extrapolation.csv")

####################### Abstract #######################
icer_list = output_icer %>% 
	filter(Strategy != 0, Smoking_ban_policy == 'Baseline') %>% 
	group_by(Strategy) %>% 
	reframe(value = median(ICER))

# Those less than 45,000 SGD/QALY
sum(icer_list$value < 45000)

# Those less than 75,000 SGD/QALY
sum(icer_list$value < 75000)

####################### Statuo quo with lung cancer screening #######################
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

# MLSOD10 vs the baseline 
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

# MLSOD10 vs 55-80, other settings same 
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

# MLSOD10 vs 50-75, other settings same 
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

# MLSOD10 vs 40% uptake, other settings same 
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

# MLSOD10 vs quinquennial screening, other settings same 
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

# MLSOD10 vs 50-85, other settings same 
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

####################### Smoking ban with lung cancer screening #######################
# MLSOD10 under immediate ban vs. MLSOD10 under status quo
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
        Base_QG = QALY_Gain, # screening
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


# MLSOD10 under immediate ban vs. MLSOD10 under status quo
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
        Base_QG = QALY_Gain, # screening
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
    QG = QALY_Gain + Base_QG, # screening
    LS = Late.cases.diagnosed - Base_LSA,
    Deaths = Total.deaths - Base_DA,
    Cost = Cost + Base_AddCost,
    OverDiagnosis = OverDiagnosis + Base_OD,
    FalsePositives = False.Positives + Base_FP,
    ICER = (Additional_cost) / QALY_Gain
  )
round(quantile((compare$QALY - ref$QALY), c(0.025, 0.5, 0.975)), 1)
round(quantile((compare$QALY - ref$QALY) / (ref$QALY_Gain + ref$Base_QG), c(0.025, 0.5, 0.975)), 2)
round(quantile((ref$Cost - compare$Cost) / 1e9, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Cost - compare$Cost) / ref$Cost * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$LS - compare$LS), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Deaths - compare$Deaths), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Deaths - compare$Deaths) / ref$Deaths * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Deaths - compare$Deaths) / (ref$Deaths_Averted + ref$Base_DA), c(0.025, 0.5, 0.975)), 2)
round(quantile((ref$OverDiagnosis - compare$OverDiagnosis), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$FalsePositives - compare$FalsePositives), c(0.025, 0.5, 0.975)), 1)

# TRS under immediate ban vs. MLSOD10 under status quo
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
        Base_QG = QALY_Gain, # screening
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


# TRS under immediate ban vs. MLSOD10 under status quo
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
        Base_QG = QALY_Gain, # screening
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
    QG = QALY_Gain + Base_QG, # screening
    LS = Late.cases.diagnosed - Base_LSA,
    Deaths = Total.deaths - Base_DA,
    Cost = Cost + Base_AddCost,
    OverDiagnosis = OverDiagnosis + Base_OD,
    FalsePositives = False.Positives + Base_FP,
    ICER = (Additional_cost) / QALY_Gain
  )
round(quantile((compare$QALY - ref$QALY), c(0.025, 0.5, 0.975)), 1)
round(quantile((compare$QALY - ref$QALY) / (ref$QALY_Gain + ref$Base_QG), c(0.025, 0.5, 0.975)), 2)
round(quantile((ref$Cost - compare$Cost) / 1e9, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Cost - compare$Cost) / ref$Cost * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$LS - compare$LS), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Deaths - compare$Deaths), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Deaths - compare$Deaths) / ref$Deaths * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Deaths - compare$Deaths) / (ref$Deaths_Averted + ref$Base_DA), c(0.025, 0.5, 0.975)), 2)
round(quantile((ref$OverDiagnosis - compare$OverDiagnosis), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$FalsePositives - compare$FalsePositives), c(0.025, 0.5, 0.975)), 1)

######################## Smoking stringent ban with lung cancer screening ################################
# MLSOD10 under stringent ban vs. MLSOD10 under status quo
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
        Base_QG = QALY_Gain, # screening
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

# MLSOD10 under stringent ban vs. MLSOD10 under status quo
compare = output_icer %>%
  filter(
    Strategy %in% c(22),
    Smoking_ban_policy == "High (80%)"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(157),
        Smoking_ban_policy == "High (80%)"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain, # screening
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
    QG = QALY_Gain + Base_QG, # screening
    LS = Late.cases.diagnosed - Base_LSA,
    Deaths = Total.deaths - Base_DA,
    Cost = Cost + Base_AddCost,
    OverDiagnosis = OverDiagnosis + Base_OD,
    FalsePositives = False.Positives + Base_FP,
    ICER = (Additional_cost) / QALY_Gain
  )
round(quantile((compare$QALY - ref$QALY), c(0.025, 0.5, 0.975)), 1)
round(quantile((compare$QALY - ref$QALY) / (ref$QALY_Gain + ref$Base_QG), c(0.025, 0.5, 0.975)), 2)
round(quantile((ref$Cost - compare$Cost) / 1e9, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Cost - compare$Cost) / ref$Cost * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$LS - compare$LS), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Deaths - compare$Deaths), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Deaths - compare$Deaths) / ref$Deaths * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Deaths - compare$Deaths) / (ref$Deaths_Averted + ref$Base_DA), c(0.025, 0.5, 0.975)), 2)
round(quantile((ref$OverDiagnosis - compare$OverDiagnosis), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$FalsePositives - compare$FalsePositives), c(0.025, 0.5, 0.975)), 1)

# TRS under immediate ban vs. TRS under status quo
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
        Base_QG = QALY_Gain, # screening
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


# TRS under stringent ban vs. TRS under status quo
compare = output_icer %>%
  filter(
    Strategy %in% c(51),
    Smoking_ban_policy == "High (80%)"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(186),
        Smoking_ban_policy == "High (80%)"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain, # screening
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
    QG = QALY_Gain + Base_QG, # screening
    LS = Late.cases.diagnosed - Base_LSA,
    Deaths = Total.deaths - Base_DA,
    Cost = Cost + Base_AddCost,
    OverDiagnosis = OverDiagnosis + Base_OD,
    FalsePositives = False.Positives + Base_FP,
    ICER = (Additional_cost) / QALY_Gain
  )
round(quantile((compare$QALY - ref$QALY), c(0.025, 0.5, 0.975)), 1)
round(quantile((compare$QALY - ref$QALY) / (ref$QALY_Gain + ref$Base_QG), c(0.025, 0.5, 0.975)), 2)
round(quantile((ref$Cost - compare$Cost) / 1e9, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Cost - compare$Cost) / ref$Cost * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$LS - compare$LS), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Deaths - compare$Deaths), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Deaths - compare$Deaths) / ref$Deaths * 100, c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$Deaths - compare$Deaths) / (ref$Deaths_Averted + ref$Base_DA), c(0.025, 0.5, 0.975)), 2)
round(quantile((ref$OverDiagnosis - compare$OverDiagnosis), c(0.025, 0.5, 0.975)), 1)
round(quantile((ref$FalsePositives - compare$FalsePositives), c(0.025, 0.5, 0.975)), 1)

######################## Discussion #######################
# Smoking ban with lung cancer screening
# MLSOD10/TRS under immediate ban vs. MLSOD10/TRS under status quo
ref = output_icer %>%
  filter(
    Strategy %in% c(22, 51),
    Smoking_ban_policy == "Baseline"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(157, 186),
        Smoking_ban_policy == "Baseline"
      ) %>%
      dplyr::select(
        Base_QG = QALY_Gain, # screening
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
    Deaths = Total.deaths - Base_DA,
    Cost = Cost + Base_AddCost,
  )

# MLSOD10/TRS under immediate ban vs. MLSOD10/TRS under status quo
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
  cbind(
  		.,
  		ref %>% 
            dplyr::select(
              Cost_baseline = Cost,
              Deaths_baseline = Deaths
            )

  )
round(quantile(100 * (1 - compare$Cost / compare$Cost_baseline), c(0.025, 0.5, 0.975)), 1)
round(quantile(100 * (1 - compare$Deaths / compare$Deaths_baseline), c(0.025, 0.5, 0.975)), 1)

# For all strategies, the ICER under immediate ban vs. status quo
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

# For all strategies, the ICER under immediate ban vs. status quo
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
round(quantile((compare$ICER - ref$ICER), c(0.025, 0.5, 0.975)), 1)

# For all strategies, the ICER under stringent ban vs. status quo
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

# For all strategies, the ICER under immediate ban vs. status quo
compare <- output_icer %>%
  filter(
    Strategy %in% c(1:135),
    Smoking_ban_policy == "High (80%)"
  ) %>%
  cbind(
    .,
    output_icer %>%
      filter(
        Strategy %in% c(136:270),
        Smoking_ban_policy == "High (80%)"
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

# FHLC
# MLSOD10 to ever-smokers + individuals with family history under immediate ban
library(dplyr)
library(tidyr)
output_icer = read.csv("../outputs/DEMOS_LC_result/outs_sim/aggressive/demos_screening_result_with_smoking_ban_policy_table_with_survival_extrapolation.csv")
output_icer_fhlc = read.csv('../outputs/DEMOS_LC_result/outs_sim/fhlc/demos_screening_result_with_smoking_ban_policy_table_fhlc_MLS10_base.csv') # Gender-specific

# The FHLC table already applies MLSOD10 to eligible ever-smokers in every row; its Strategy ID
# indexes the design applied to individuals with a family history. Extend MLSOD10 (annual, 50-80,
# 100% uptake) to this group as well: male 22, female 157
ban = "Immediate Ban (100%)"
fh_m = output_icer_fhlc %>% filter(gender == "male", Strategy == 22, Smoking_ban_policy == ban) %>% arrange(Seed)
fh_f = output_icer_fhlc %>% filter(gender == "female", Strategy == 157, Smoking_ban_policy == ban) %>% arrange(Seed)

# MLSOD10 in eligible ever-smokers: male 22, female 157
es_m = output_icer %>% filter(Strategy == 22, Smoking_ban_policy == ban) %>% arrange(Seed)
es_f = output_icer %>% filter(Strategy == 157, Smoking_ban_policy == ban) %>% arrange(Seed)
stopifnot(all(fh_m$Seed == fh_f$Seed), all(fh_m$Seed == es_m$Seed), all(fh_m$Seed == es_f$Seed))

fh_QG = fh_m$QALY_Gain + fh_f$QALY_Gain
fh_AC = fh_m$Additional_cost + fh_f$Additional_cost
es_QG = es_m$QALY_Gain + es_f$QALY_Gain
es_AC = es_m$Additional_cost + es_f$Additional_cost

# Family-history add-on vs. screening eligible ever-smokers alone
round(quantile(fh_QG, c(0.025, 0.5, 0.975)), 1)
round(quantile(fh_AC / 1e6, c(0.025, 0.5, 0.975)), 1)
round(quantile(fh_AC / fh_QG, c(0.025, 0.5, 0.975)), 1)

# Combined program under immediate ban vs. no additional screening under immediate ban
round(quantile((es_AC + fh_AC) / (es_QG + fh_QG), c(0.025, 0.5, 0.975)), 1)
