library(dplyr)
# rm(list = ls())
# gc()

branch <- c("aggressive", "conservative")[1]
policy <- c("Baseline", "Mild", "Moderate", "Stringent", "Immediate_Ban")[5]

lc_result <- NULL
smoke_result <- NULL

for (i in 1:100) {
  thislc <- readRDS(paste0("../outputs/lc_psa_output/run_seed_", i, "_policy_", policy, ".rds"))[["ind_mort"]] %>%
    mutate(Seed = !!i)
  lc_result <- bind_rows(lc_result, thislc)

  thissmoke <- readRDS(paste0("../outputs/smoke_psa_output/run_seed_", i, "_policy_", policy, ".rds"))
  smoke_result <- rbind(smoke_result, thissmoke)
}
print(nrow(lc_result))
print(nrow(smoke_result))

# ==== For lung cancer incidence and mortality projection ====
# 1) Get a scalar end year from your data
end_year <- 2050

# 2) Build breaks aligned to 5-year bins starting at 1988
#    Intervals will be [1988,1993), [1993,1998), ... with labels like 1988-1992
k_max <- floor((end_year - 1988) / 5)
breaks_seq <- seq(1988, 1988 + 5 * (k_max + 1), by = 5)

# 3) Generate matching labels automatically
labels_seq <- sprintf("%d-%d", breaks_seq[-length(breaks_seq)], breaks_seq[-1] - 1)

lc_result <- lc_result %>%
  mutate(
    period = cut(
      year,
      breaks = breaks_seq,
      right  = FALSE, # [a, b)
      labels = labels_seq
    )
  )

# Get total number each year
lc_result_total <- lc_result %>%
  group_by(year, Seed, period) %>%
  reframe(ind = sum(ind), death = sum(death)) %>%
  tibble::add_column(gender = -1, .before = 1) %>%
  dplyr::select(colnames(lc_result))

# Annual ind and death with CI
lc_result %>%
  bind_rows(lc_result_total) %>%
  group_by(gender, year) %>%
  reframe(
    mean_ind = mean(ind), median_ind = median(ind), low_ind = quantile(ind, 0.025), high_ind = quantile(ind, 0.975),
    mean_death = median(death), median_death = median(death), low_death = quantile(death, 0.025), high_death = quantile(death, 0.975)
  ) %>%
  saveRDS(paste0("../outputs/DEMOS_LC_result/outs_sim/", branch, "/", tolower(policy), "/demos_lc_ind_death_with_ci_", tolower(policy), ".rds"))


# Total ind and death with CI every 5 years
lc_result <- lc_result %>%
  bind_rows(lc_result_total) %>%
  group_by(gender, period, Seed) %>%
  reframe(incidence = sum(ind), mortality = sum(death)) %>%
  group_by(gender, period) %>%
  reframe(
    mean_Incidence = mean(incidence), median_Incidence = median(incidence), low_Incidence = quantile(incidence, 0.025), high_Incidence = quantile(incidence, 0.975),
    mean_Mortality = mean(mortality), median_Mortality = median(mortality), low_Mortality = quantile(mortality, 0.025), high_Mortality = quantile(mortality, 0.975)
  ) %>%
  mutate(gender = case_when(
    gender == -1 ~ "total",
    gender == 0 ~ "male",
    T ~ "female"
  ))

data <- list()

incidence_replacement <- filter(lc_result, period %in% c("2008-2012", "2013-2017", "2018-2022")) %>%
  rename(DEMOS = median_Incidence, DEMOS_low = low_Incidence, DEMOS_high = high_Incidence) %>%
  dplyr::select(gender, period, DEMOS, DEMOS_low, DEMOS_high)
incidence_replacement$Observed <- c(6556, 7914, 9293, 4292, 5069, 5759, 2264, 2845, 3534)
data[["Incidence"]] <- incidence_replacement

mortality_replacement <- filter(lc_result, period %in% c("2008-2012", "2013-2017", "2018-2022")) %>%
  rename(DEMOS = median_Mortality, DEMOS_low = low_Mortality, DEMOS_high = high_Mortality) %>%
  dplyr::select(gender, period, DEMOS, DEMOS_low, DEMOS_high)
mortality_replacement$Observed <- c(5238, 5890, 5986, 3682, 4046, 3968, 1556, 1844, 2018)
data[["Mortality"]] <- mortality_replacement
saveRDS(data, paste0("../outputs/DEMOS_LC_result/outs_sim/", branch, "/", tolower(policy), "/demos_lc_ind_death_with_ci_by_5_yrs_", tolower(policy), ".rds"))


# ==== For smoking prevalence CI ====
row.names(smoke_result) <- NULL
saveRDS(smoke_result, paste0("../outputs/DEMOS_LC_result/outs_sim/", branch, "/", tolower(policy), "/smoking_ci_", tolower(policy), ".rds"))
