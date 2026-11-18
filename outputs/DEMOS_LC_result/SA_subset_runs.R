## This code script is for sensitivity analysis of the number of simulation runs
## used in strategy selection. The original selection procedure (plot.R, radar
## plot section) uses all 100 runs. Here it is repeated on random subsets of
## 50 and 75 runs (100 repetitions each), and the selected strategies are
## compared with those selected from all 100 runs.
library(dplyr)
library(parallel)

rm(list = ls())
gc()

source('code/functions.R')

n_cores <- parallel::detectCores()
if (is.na(n_cores) || n_cores < 1) n_cores <- 1L

# Functions in `code/functions.R` call mclapply(..., mc.cores = detectCores())
# internally. Since repetitions are parallelised here, force those inner calls
# to run serially to avoid nested forking (global lookup finds this first).
detectCores <- function(...) 1L

# Directory
sim.dir = 'outs_sim/'
out.dr = 'outs/'

# Subset settings
subset_sizes = c(50, 75)
n.rep = 100
set.seed(42)

# Label settings (same as plot.R)
gender = c('Male', 'Female'); n.gender = length(gender)
scenario_name = c('Status quo', 'Mild', 'Moderate', 'Stringent', 'Immediate ban'); n.scenario = length(scenario_name)
scenario_proj = c('Baseline', 'Mild (20%)', 'Moderate (50%)', 'High (80%)', 'Immediate Ban (100%)')
metrics = c('QALY', 'LSA', 'Deaths_Averted', 'Cost_Savings','OD_rate','FP_rate'); n.metrics = length(metrics)
radar_metrics = paste0(metrics, '_scaled')
strategy_name = c('MLSOD10', 'Most cost-effective', 'Lowest-overdiagnosis', 'TRS'); n.strategy = length(strategy_name)

# Load data
ver = c('aggressive', 'conservative')[1]
proj = read.csv(paste0(sim.dir, ver, '/demos_screening_result_with_smoking_ban_policy_table_',
                       ifelse(ver == 'aggressive','with','without'),'_survival_extrapolation.csv'))

proj_sb = proj_clean(proj, scale_all = F, new_derive = T, agg_by_seed = T)
proj_sb0 = proj_clean(proj, scale_all = F, new_derive = T, agg_by_seed = F, scale = F)
n.sim = max(proj_sb0$Seed)
all_seeds = sort(unique(proj_sb0$Seed))

# Per-seed cleaned data for TOPSIS (identical to the per-seed proj_clean call in
# plot.R; it only depends on the seed, so clean each seed once)
proj_by_seed_clean = mclapply(split(proj, proj$Seed), function(proj_seed) {
  proj_clean(proj_seed, scale_all = F, new_derive = F, agg_by_seed = F, scale = F)
}, mc.cores = n_cores)

# Same decisions as pareto_frontier_multi_seed(), but splits the data once
# instead of filtering inside the double loop
pareto_frontier_multi_seed_fast <- function(df, cols, maximize, idx_var = 'Strategy') {
  idx_list = unique(df[[idx_var]])
  mats = lapply(idx_list, function(j) as.matrix(df[df[[idx_var]] == j, cols, drop = FALSE]))
  sgn = ifelse(maximize, 1, -1)
  m = length(idx_list)
  dominated = rep(FALSE, m)
  for (i in 1:m) {
    if (!dominated[i]) {
      for (j in 1:m) {
        if (i != j && !dominated[j]) {
          delta = sweep(mats[[j]] - mats[[i]], 2, sgn, '*')
          res1 = vapply(seq_along(cols), function(k) t.test(delta[, k], alternative = 'greater')$p.value < 0.05, logical(1))
          res2 = vapply(seq_along(cols), function(k) t.test(delta[, k], alternative = 'less')$p.value < 0.05, logical(1))
          if (sum(res1) > 0 && sum(res2) == 0) {
            dominated[i] = TRUE
            break
          }
        }
      }
    }
  }
  idx_list[!dominated]
}

# Original selection procedure applied to a given set of seeds.
# Seeds are renumbered 1..k because into_mat() iterates over Seed = 1..max(Seed).
select_strategies = function(seeds) {
  seed_map = setNames(seq_along(seeds), seeds)
  sb0 = proj_sb0 %>%
    filter(Seed %in% seeds) %>%
    mutate(Seed = unname(seed_map[as.character(Seed)])) %>%
    arrange(Seed)
  k.sim = length(seeds)

  lapply(seq_along(scenario_proj), function(m) {
    lapply(0:1, function(i) {
      dat1 = proj_sb %>% filter(screen_gender == i, Smoking_ban_policy == scenario_proj[m])
      dat2 = sb0 %>% filter(screen_gender == i, Smoking_ban_policy == scenario_proj[m])

      eligible = dat2 %>%
        group_by(Strategy) %>%
        summarise(p_val = t.test(OD_rate - 10, alternative = 'greater')$p.value) %>%
        filter(p_val > 0.05) %>%
        pull(Strategy)
      life_saving = find_optim(dat2 %>% filter(Strategy %in% eligible), 'Deaths_Averted', min = F,
                               strategy_idx = dat2 %>% filter(Strategy %in% eligible) %>% pull(Strategy) %>% unique)
      most_cea = find_optim(dat2, 'ICER', min = T, strategy_idx = dat1$Strategy)
      od_control = find_optim(dat2, 'OD_rate', min = T, strategy_idx = dat1$Strategy)

      # TOPSIS
      is_front = pareto_frontier_multi_seed_fast(dat2 %>% filter(Strategy %in% eligible), cols = metrics,
                                                 maximize = c(rep(T, 3), rep(F, n.metrics - 3)))
      eligible = intersect(eligible, is_front)
      topsis_idx = lapply(seeds, function(s) {
        d = proj_by_seed_clean[[as.character(s)]] %>%
          filter(screen_gender == i, Smoking_ban_policy == scenario_proj[m], Strategy > 0) %>%
          filter(Strategy %in% eligible) %>%
          proj_clean(scale = T, scale_approach = 'lognormal') %>% filter(Strategy %in% eligible)
        compute_topsis_scores(df = d, decision_cols = radar_metrics, beneficial = rep(TRUE, n.metrics))
      }) %>% do.call('rbind', .) %>% as.matrix
      topsis = eligible[which.max(colMeans(topsis_idx))]

      data.frame(
        policy_scenario = scenario_name[m],
        gender = gender[i + 1],
        strategy_type = strategy_name,
        Strategy = c(life_saving, most_cea, od_control, topsis)
      )
    }) %>% bind_rows()
  }) %>% bind_rows()
}

# Reference: selection with all 100 runs ---------------------------------------
cat('Selecting strategies with all', n.sim, 'runs\n')
t0 = Sys.time()
ref = select_strategies(all_seeds) %>% rename(Strategy_ref = Strategy)
cat('Done in', format(Sys.time() - t0), '\n')
print(ref)

# Subset repetitions ------------------------------------------------------------
# Draw all subsets up front so results are reproducible regardless of cores.
# The same subset of runs is used across the 5 tobacco control scenarios.
subset_draws = lapply(subset_sizes, function(n) {
  lapply(1:n.rep, function(r) sort(sample(all_seeds, n)))
})
names(subset_draws) = subset_sizes

jobs = expand.grid(rep = 1:n.rep, size = subset_sizes)
cat('Running', nrow(jobs), 'subset selections on', n_cores, 'cores\n')
t0 = Sys.time()
subset_res = mclapply(seq_len(nrow(jobs)), function(j) {
  size = jobs$size[j]; r = jobs$rep[j]
  seeds = subset_draws[[as.character(size)]][[r]]
  res = tryCatch(select_strategies(seeds), error = function(e) {
    message('size ', size, ' rep ', r, ' failed: ', conditionMessage(e)); NULL
  })
  if (!is.null(res)) res = res %>% mutate(subset_size = size, rep = r, seeds = paste(seeds, collapse = ';'))
  res
}, mc.cores = n_cores, mc.preschedule = FALSE) %>% bind_rows()
cat('Done in', format(Sys.time() - t0), '\n')

subset_res = subset_res %>%
  left_join(ref, by = c('policy_scenario', 'gender', 'strategy_type')) %>%
  mutate(same_as_ref = Strategy == Strategy_ref)

# Attach screening criteria for readability
strategy_info = proj_sb %>%
  distinct(Strategy, min_age, max_age, uptake, pack_yrs, quitting_t, freq) %>%
  mutate(criteria = sprintf('%d-%d, %d%%, freq %d', min_age, max_age, round(uptake * 100), freq)) %>%
  select(Strategy, criteria)

# Summary ------------------------------------------------------------------------
# Agreement with the 100-run selection, per subset size / scenario / gender / type
agreement = subset_res %>%
  group_by(subset_size, policy_scenario, gender, strategy_type, Strategy_ref) %>%
  summarise(n_rep = n(), n_same = sum(same_as_ref), pct_same = 100 * mean(same_as_ref), .groups = 'drop') %>%
  left_join(strategy_info, by = c('Strategy_ref' = 'Strategy')) %>%
  rename(criteria_ref = criteria) %>%
  mutate(policy_scenario = factor(policy_scenario, scenario_name),
         strategy_type = factor(strategy_type, strategy_name)) %>%
  arrange(subset_size, policy_scenario, gender, strategy_type)

# Distribution of the strategies selected across repetitions
selection_freq = subset_res %>%
  count(subset_size, policy_scenario, gender, strategy_type, Strategy, same_as_ref, name = 'n_rep') %>%
  left_join(strategy_info, by = 'Strategy') %>%
  mutate(policy_scenario = factor(policy_scenario, scenario_name),
         strategy_type = factor(strategy_type, strategy_name)) %>%
  arrange(subset_size, policy_scenario, gender, strategy_type, desc(n_rep))

# Overall: % of repetitions where ALL 40 selections (5 scenarios x 2 genders x
# 4 strategy types) match the 100-run selection
overall = subset_res %>%
  group_by(subset_size, rep) %>%
  summarise(all_same = all(same_as_ref), n_diff = sum(!same_as_ref), .groups = 'drop') %>%
  group_by(subset_size) %>%
  summarise(n_rep = n(), pct_all_same = 100 * mean(all_same),
            mean_n_diff = mean(n_diff), max_n_diff = max(n_diff), .groups = 'drop')

overall_by_type = subset_res %>%
  group_by(subset_size, strategy_type) %>%
  summarise(pct_same = 100 * mean(same_as_ref), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = subset_size, values_from = pct_same, names_prefix = 'pct_same_n')

options(width = 200)
cat('\n===== Overall agreement with the 100-run selection =====\n')
print(as.data.frame(overall))
cat('\n===== Agreement by strategy type (% of all scenario/gender/rep selections) =====\n')
print(as.data.frame(overall_by_type))
cat('\n===== Agreement by subset size, scenario, gender and strategy type =====\n')
print(as.data.frame(agreement), row.names = FALSE)
cat('\n===== Selections that differ from the 100-run selection =====\n')
print(as.data.frame(selection_freq %>% filter(!same_as_ref)), row.names = FALSE)

write.csv(subset_res, file.path(out.dr, paste0('subset_runs_selection_by_rep_', ver, '.csv')), row.names = FALSE)
write.csv(agreement, file.path(out.dr, paste0('subset_runs_agreement_', ver, '.csv')), row.names = FALSE)
write.csv(selection_freq, file.path(out.dr, paste0('subset_runs_selection_freq_', ver, '.csv')), row.names = FALSE)
