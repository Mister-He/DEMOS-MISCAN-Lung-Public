## This code script is for Sensitivity analysis of TOPSIS weights 
library(dplyr)
library(parallel)
library(grid)
library(viridis)
library(gtools)

rm(list = ls())
gc()

# Source TOPSIS calculation function from `code/functions.R`
source('code/functions.R')

n_cores <- detectCores()
if (is.na(n_cores)) {
  n_cores <- suppressWarnings(as.integer(
    system('getconf _NPROCESSORS_ONLN', intern = TRUE)
  ))
}
if (length(n_cores) != 1 || is.na(n_cores) || n_cores < 1) n_cores <- 1L

# Directory
sim.dir = 'outs_sim/'
plot.dr='outs/'

# Key parameters (QG, AddCost, DA, OD, LSA, FP)
set.seed(42)
generate_balanced_weights <- function(
  benefit_idx = c(1, 3, 5),
  burden_idx  = c(2, 4, 6),
  min_weight  = 0.1
) {
  w <- numeric(6)

  benefit_remainder <- 0.5 - length(benefit_idx) * min_weight
  burden_remainder <- 0.5 - length(burden_idx) * min_weight

  if (benefit_remainder < 0 || burden_remainder < 0) {
    stop('min_weight is too large for weights that sum to 0.5 per group')
  }

  w[benefit_idx] <- min_weight + benefit_remainder * rdirichlet(
    1, rep(1, length(benefit_idx))
  )

  w[burden_idx] <- min_weight + burden_remainder * rdirichlet(
    1, rep(1, length(burden_idx))
  )

  return(w)
}

try_weights = list(
  'Randomly-generated-1' = generate_balanced_weights(),
  'Randomly-generated-2' = generate_balanced_weights(),
  'Randomly-generated-3' = generate_balanced_weights(),
  'Randomly-generated-4' = generate_balanced_weights(),
  'Randomly-generated-5' = generate_balanced_weights(),
  'Randomly-generated-6' = generate_balanced_weights(),
  'Randomly-generated-7' = generate_balanced_weights(),
  'Randomly-generated-8' = generate_balanced_weights(),
  'Randomly-generated-9' = generate_balanced_weights(),
  'Randomly-generated-10' = generate_balanced_weights()
)

stopifnot(all(vapply(try_weights, min, numeric(1)) >= 0.1))

# Label settings
gender = c('Male', 'Female')
n.gender = length(gender)

scenario_name = c('Status quo', 'Mild', 'Moderate', 'Stringent', 'Immediate ban'); n.scenario = length(scenario_name)
scenario_proj = c('Baseline', 'Mild (20%)', 'Moderate (50%)', 'High (80%)', 'Immediate Ban (100%)')

freq_name = c('Annual', 'Biennual', 'Triennial', 'Quadrennial', 'Quinquennial'); n.freq = length(freq_name)
freq_name_s = c('Ann', 'Bi','Tri','Quad','Quin')

uptakes = c(0.4, 0.7, 1)*100; n.uptake = length(uptakes)
metrics = c('QALY', 'LSA', 'Deaths_Averted', 'Cost_Savings','OD_rate','FP_rate'); n.metrics = length(metrics)

radar_metrics = paste0(metrics, '_scaled')
metric_name=c('QALY gains', 'Late-stage cancer averted', 'Deaths averted', 'Additional costs','Overdiagnosis rate','False-positive rate')
metric_name_short = c('QG', 'LSA', 'DA', 'AddCost', 'OD',  'FP')

strategy_name = c('MLSOD10', 'Most cost-effective', 'Lowest-overdiagnosis', 'TRS'); n.strategy = length(strategy_name)
strategy_name1 = c('MLSOD10', 'Most\ncost-effective', 'Lowest\noverdiagnosis', 'TRS')

inc_death_name = c('Incidence', 'Mortality')
smoke_prev_name = 'Smoker (%)'

screen_vars = c('min_age','max_age','uptake','freq'); n.screen_vars = length(screen_vars)
screen_varname = c('Minimum age (yr)', 'Maximum age (yr)', 'Uptake (%)', 'Frequency')

range_list = list(
  min_age = c(50, 55, 60), 
  max_age = c(75, 80, 85), 
  uptake = c(0.4, 0.7, 1)*100,
  # freq = 1:5
  freq = freq_name
)

# Load data
#projected incidence & death by gender and scenario from 2009 to 2050: 0 for male, -1 for total
ver = c('aggressive', 'conservative')[1]
scenario_file = c('Baseline', 'Mild', 'Moderate', 'Stringent', 'Immediate_Ban')
outcomes = lapply(scenario_file, function(m){
  out = readRDS(paste0(sim.dir, ver, '/',tolower(m),'/demos_lc_ind_death_with_ci_',tolower(m),'.rds'))
  inc_proj=lapply(-1:1, function(i){
    dat=out%>%
      filter(gender==i)%>%
      select(all_of(paste0(c('mean', 'low','high'), '_ind')))%>%
      as.matrix()
    dat/1e3
  })
  death_proj=lapply(-1:1, function(i){
    dat=out%>%
      filter(gender==i)%>%
      select(all_of(paste0(c('mean', 'low','high'), '_death')))%>%
      as.matrix()
    dat/1e3
  })
  list(inc_proj = inc_proj, death_proj = death_proj )
})

#projection by 2050 (screening strategies)
proj = proj_m = read.csv(paste0(sim.dir, ver, '/demos_screening_result_with_smoking_ban_policy_table_',
                       ifelse(ver == 'aggressive','with','without'),'_survival_extrapolation.csv'))


#color scheme-------
color_list = list(
  female_fit = 'firebrick', female_obs = 'purple', #female_obs = 'orchid3', 
  male_fit = 'darkslateblue', male_obs = 'cadetblue3',
  tot_fit = 'black', tot_obs = 'darkorange',
  # stage_fit = viridis(6, option='C')[1:4+1],
  stage_fit = viridis(6, option = 'F')[5:2], stage_obs = c('royalblue','purple','red1','sienna1')%>%rev,
  freq_g = cbind(
    viridis(1+n.freq, option = 'G')[1:n.freq]%>%rev, #male
    viridis(4+n.freq, option = 'F')[1:n.freq + 3]%>%rev #female
  ),
  scenario_g = cbind(
    viridis(1+n.scenario, option = 'G')[n.scenario:1], #male
    viridis(1+n.scenario, option = 'F')[n.scenario:1] #female
  ),
  scenario = viridis(n.scenario+3, option = 'C')[n.scenario:1+1], 
  metrics = c('rosybrown4', 'firebrick2', 'purple3', 'sienna3', 'royalblue2', 'goldenrod2'),
  strategy = c('purple', 'rosybrown4', 'royalblue3','goldenrod2'),
  strategy.g = cbind(
    c('purple', 'rosybrown4', 'royalblue3','goldenrod2'),
    c('hotpink3', 'yellow4', 'blue4','darkorange1')
  ),
  single_obs = 'royalblue4', 
  sens_levels = c('goldenrod1', 'royalblue4', 'firebrick3')
)


# TOPSIS normalized metrics by weight combination ----------------------------
radar_idx = c(1, 4, 3, 5, 2, 6)
proj_sb = proj_clean(proj, scale_all = F, new_derive = T, agg_by_seed = T)
proj_sb0 = proj_clean(proj, scale_all = F, new_derive = T, agg_by_seed = F, scale = F)
n.sim = max(proj_sb0$Seed)
if (FALSE) {
  cat('Start drawing radar plot ', try_weights_idx,'\n')
  radar_dat = lapply(seq_along(scenario_proj), function(m){
    #proj_sb = proj_clean(proj, scenario0 = m)
    lapply(0:1, function(i){
      
      dat1 = proj_sb %>% filter(screen_gender == i, Smoking_ban_policy == scenario_proj[m])
      dat2 = proj_sb0 %>% filter(screen_gender == i, Smoking_ban_policy == scenario_proj[m])
      eligible = dat2%>%
        group_by(Strategy)%>%
        summarise(p_val = t.test(OD_rate-10, alternative = 'greater')$p.value)%>%
        filter(p_val > 0.05)%>%
        pull(Strategy)
      # eligible = dat1%>%filter(OD_rate_l<10)%>%pull(Strategy)
      life_saving = find_optim(dat2%>%filter(Strategy%in%eligible), 'Deaths_Averted', min = F, 
                               strategy_idx = dat2%>%filter(Strategy%in%eligible)%>%pull(Strategy)%>%unique)
      most_cea = find_optim(dat2, 'ICER', min = T, strategy_idx = dat1$Strategy)
      od_control = find_optim(dat2, 'OD_rate', min = T, strategy_idx = dat1$Strategy)
      #TOPSIS
      is_front = pareto_frontier_multi_seed(dat2%>%filter(Strategy %in% eligible), cols = metrics, maximize = c(rep(T, 3), rep(F, n.metrics-3)))
      eligible = intersect(eligible, is_front)
      # n.eligible = length(eligible)
      topsis_idx = mclapply(1:n.sim, function(s){
        dat2 = proj_clean(proj%>%filter(Seed == s), 
                          scale_all = F, new_derive = F, agg_by_seed = F, scale = F)%>%
          filter(screen_gender == i, Smoking_ban_policy == scenario_proj[m], Strategy>0)%>%
          filter(Strategy %in% eligible)%>%
          proj_clean(scale = T, scale_approach = 'lognormal')%>%filter(Strategy %in% eligible)
        scores = compute_topsis_scores(
          df            = dat2, #%>% 
          weights = try_weights[[try_weights_idx]],
          decision_cols = radar_metrics,
          beneficial    = rep(TRUE, n.metrics)
        )
        scores
      }, mc.cores = n_cores)%>%do.call('rbind',.)%>%as.matrix#%>%
      topsis = eligible[which.max(colMeans(topsis_idx))]
      idx = c(life_saving, most_cea, od_control, topsis)
      idx_strategy = lapply(idx, function(i){
        which(dat1$Strategy == i)
      })%>%unlist
      metric_val = dat1[idx_strategy, ] %>% select (all_of(radar_metrics)) %>% as.matrix()
      metric_val0 = dat1[idx_strategy, ] %>% select (all_of(c(metrics,'ICER'))) %>% as.matrix()
      strategy = dat1[idx_strategy, ] %>% select (Strategy, min_age, max_age, uptake, pack_yrs, quitting_t, freq)
      list(metrics = metric_val, metrics_raw = metric_val0, strategy = strategy)
    })
  })
  
  #radar plot & strategy for baseline------------
  # m = 1 # baseline
  # clr = color_list$strategy.g
  # name_all = lapply(gender, function(i) paste0(strategy_name, ' (', i, ')'))%>%unlist
  # png(paste0(plot.dr,'select_',scenario_file[m],'_',ver,'_',names(try_weights)[[try_weights_idx]],'.png'),width=25, height=12, units='cm', res=300,pointsize=10)
  # pushViewport(plotViewport(c(1,1,1,15)))
  # pushViewport(viewport(layout = grid.layout(nrow=3,ncol=n.strategy)))
  # for(k in 1:2){
  #   for(s in 1:n.strategy){
  #     margin=c(1, 1, 1, 2.5)
  #     pushViewport(viewport(layout.pos.row = k,layout.pos.col = s))
  #     plot_radar(radar_dat[[m]][[k]]$metrics[s,radar_idx], col=clr[s,k], size = 0.85, 
  #                shade = T, shade_alpha = 0.2,
  #                line_alpha = 0.8, line_type = 1, plot_name_bold = F,
  #                label = metric_name_short[radar_idx], plot_name = paste0('(',letters[s+(k-1)*n.strategy], ')'))
  #     if(s==1){
  #       plot_xy(xmargin = F, ymargin = F, 
  #               y_name = gender[k], y_name_pos = 1, rot_y = T,
  #               plot_x = F, plot_y = F)
  #     }
  #     popViewport()
  #   }
  # }
  # for(s in 1:n.screen_vars){
  #   ylab = range_list[[screen_vars[s]]]; ytk = ytk1 = seq_along(ylab)
  #   xtk = 0:n.strategy; xtk1 = 1: n.strategy - 0.5; xlab = strategy_name1
  #   # xtk = xtk1 = 1:n.strategy - 0.5; xlab= rep('',n.screen_vars)
  #   yrange = c(0, length(ylab) ); xrange = c(0, n.strategy)
  #   margin=c(0.5, 2, 1.5, 1)
  #   pushViewport(viewport(layout.pos.row = 3,layout.pos.col = s))
  #   plot_data = lapply(1:2, function(k){
  #     x = radar_dat[[m]][[k]]$strategy[[screen_vars[s]]]
  #     x1 = lapply(x, function(i){
  #       if(screen_vars[s] == 'uptake'){
  #         which((ylab/100) == i)
  #       }else if(screen_vars[s] == 'freq'){
  #         #length(ylab) + 1 - i
  #         i
  #       }else{
  #         which(ylab == i)
  #       }
  #     })%>%unlist
  #   })
  #   plot_bar(plot_data, col = clr, width_between = 0.4)
  #   y_text = lab_size_y = ifelse(screen_vars[s] == 'freq', '', screen_varname[s]); x_text = ''
  #   plot_xy(xmargin = F, ymargin = T, 
  #           plot_label = paste0('(',letters[n.strategy*2 + s],')'),
  #           closer_y = 0.6, plot_label_above = T, lab_size_x = 0.35, lab_pos_x = 0.8,
  #           customize_yaxis = T, customize_xaxis = T,lab_pos_y = 0.5,
  #           lab_size_y = ifelse(screen_vars[s] == 'freq', 0.6, 1))
  #   popViewport()
  # }
  # popViewport() 
  # plot_legend(name_all, col=as.vector(clr), topright = F, right_pos = 2, line_length = 1.5,
  #             n.lines = n.strategy * 2)
  # popViewport()
  # dev.off()
  #
  #
  # #strategies for all five scenarios----------
  # clr = color_list$strategy.g
  # name_all = lapply(gender, function(i) paste0(strategy_name, ' (', i, ')'))%>%unlist
  # png(paste0(plot.dr,'select_all_',ver,'_',names(try_weights)[[try_weights_idx]],'.png'),width=31, height=18, units='cm', res=300,pointsize=10)
  # pushViewport(plotViewport(c(0,2,2,15)))
  # pushViewport(viewport(layout = grid.layout(nrow=4,ncol=1)))
  # for(k in 1:2){
  #   grid.text(gender[k], unit(-0.5, 'lines'), 1.25 - k/2, gp=gpar(font=2, cex = 1.5), rot = 90)
  #   #radar plot
  #   margin=c(1, 0.5, 0.5, 2)
  #   pushViewport(viewport(layout.pos.row = k*2 - 1,layout.pos.col = 1))
  #   pushViewport(viewport(layout = grid.layout(nrow=1,ncol=n.scenario)))
  #   for(m in 1:n.scenario){
  #     pushViewport(viewport(layout.pos.row = 1,layout.pos.col = m))
  #     plot_radar(radar_dat[[m]][[k]]$metrics[,radar_idx], col=clr[,k], size = 0.85, 
  #                shade = c(rep(F, n.strategy-1), T), shade_alpha = 0.1,
  #                line_alpha = c(rep(0.6, n.strategy-1), 0.8),
  #                line_type = c(2:n.strategy, 1), 
  #                label = metric_name_short[radar_idx], plot_name_bold = F,
  #                plot_name = paste0('(',letters[(k-1) * (n.scenario + n.screen_vars)+m], ') ', scenario_name[m]))
  #     popViewport()
  #   }
  #   popViewport()
  #   popViewport()
  #   pushViewport(viewport(layout.pos.row = k*2,layout.pos.col = 1))
  #   pushViewport(viewport(layout = grid.layout(nrow=1,ncol=n.screen_vars)))
  #   for(s in 1:n.screen_vars){
  #     ylab = range_list[[screen_vars[s]]]; ytk = ytk1 = seq_along(ylab)
  #     xtk = 0:n.strategy; xtk1 = 1: n.strategy - 0.5; xlab = rep('',n.strategy); xlab = strategy_name1
  #     yrange = c(0, length(ylab) ); xrange = c(0, n.strategy)
  #     margin=c(3, 2, 2, 1)
  #     pushViewport(viewport(layout.pos.row = 1,layout.pos.col = s))
  #     plot_data = lapply(1:n.scenario, function(m){
  #       x = radar_dat[[m]][[k]]$strategy[[screen_vars[s]]]
  #       x1 = lapply(x, function(i){
  #         if(screen_vars[s] == 'uptake'){
  #           which((ylab/100) == i)
  #         }else if(screen_vars[s] == 'freq'){
  #           # length(ylab) + 1 - i
  #           i
  #         }else{
  #           which(ylab == i)
  #         }
  #       })%>%unlist
  #     })
  #     plot_bar(plot_data, col = clr[,k], width_between = 0.3, same_col = T)
  #     y_text = ''; x_text = ''
  #     plot_xy(xmargin = F, ymargin = T, 
  #             plot_label = paste0('(',letters[(k-1) * (n.scenario + n.screen_vars)+s + n.scenario],') ', screen_varname[s]),
  #             closer_y = 0.6, plot_label_above = T, lab_size_x = 0.5, lab_pos_x = 0.8,
  #             customize_yaxis = T, customize_xaxis = T,lab_pos_y = 0.5, lab_size_y = ifelse(screen_vars[s] == 'freq', 0.6, 1))
  #     popViewport()
  #   }
  #   popViewport()
  #   popViewport()
  # }
  # popViewport() 
  # plot_legend(name_all, col=clr%>%as.vector(), topright = F, right_pos = 2, line_length = 1.5,
  #             n.lines = n.metrics * 2, line_type = rep(c(2:n.strategy, 1),2))
  # popViewport()
  # dev.off()
  cat('Finish drawing radar plot ', try_weights_idx,'\n')
}

# Keep only the six normalized metrics for the strategy selected by TOPSIS.
# TOPSIS is selected separately by policy scenario and gender, so those
# identifiers are included to distinguish rows in the final table.
# Clean each seed once because this step does not depend on the sampled weights.
proj_by_seed = split(proj, proj$Seed)
proj_by_seed_clean = mclapply(proj_by_seed, function(proj_seed) {
  proj_clean(
    proj_seed,
    scale_all = F, new_derive = F, agg_by_seed = F, scale = F
  )
}, mc.cores = n_cores)

# Eligibility and scaled seed data also do not depend on the sampled weights,
# so cache them once for each policy/gender combination.
topsis_inputs = lapply(seq_along(scenario_proj), function(m) {
  lapply(0:1, function(i) {
    dat1 = proj_sb %>%
      filter(screen_gender == i, Smoking_ban_policy == scenario_proj[m])
    dat2 = proj_sb0 %>%
      filter(screen_gender == i, Smoking_ban_policy == scenario_proj[m])

    eligible = dat2 %>%
      group_by(Strategy) %>%
      summarise(
        p_val = t.test(OD_rate - 10, alternative = 'greater')$p.value,
        .groups = 'drop'
      ) %>%
      filter(p_val > 0.05) %>%
      pull(Strategy)

    is_front = pareto_frontier_multi_seed(
      dat2 %>% filter(Strategy %in% eligible),
      cols = metrics,
      maximize = c(rep(T, 3), rep(F, n.metrics - 3))
    )
    eligible = intersect(eligible, is_front)

    seed_data = lapply(seq_len(n.sim), function(s) {
      proj_by_seed_clean[[as.character(s)]] %>%
        filter(
          screen_gender == i,
          Smoking_ban_policy == scenario_proj[m],
          Strategy > 0,
          Strategy %in% eligible
        ) %>%
        proj_clean(scale = T, scale_approach = 'lognormal') %>%
        filter(Strategy %in% eligible)
    })

    list(dat1 = dat1, eligible = eligible, seed_data = seed_data)
  })
})

topsis_metric_results = lapply(seq_along(try_weights), function(try_weights_idx) {
  weight_name = names(try_weights)[try_weights_idx]

  lapply(seq_along(scenario_proj), function(m) {
    lapply(0:1, function(i) {
      input = topsis_inputs[[m]][[i + 1]]
      dat1 = input$dat1
      eligible = input$eligible

      # Compute scores for every simulation seed and select the strategy with
      # the highest mean TOPSIS score.
      topsis_scores = lapply(input$seed_data, function(dat2_seed) {
        compute_topsis_scores(
          df = dat2_seed,
          weights = try_weights[[try_weights_idx]],
          decision_cols = radar_metrics,
          beneficial = rep(TRUE, n.metrics)
        )
      }) %>%
        do.call('rbind', .) %>%
        as.matrix()

      topsis_index = eligible[which.max(colMeans(topsis_scores, na.rm = TRUE))]

      # proj_sb contains the normalized/scaled values used for the radar
      # representation. Select only the row for the TOPSIS strategy.
      topsis_metrics = dat1 %>%
        filter(Strategy == topsis_index) %>%
        select(all_of(radar_metrics)) %>%
        slice(1)

      topsis_strategy = dat1 %>%
        filter(Strategy == topsis_index) %>%
        select(min_age, max_age, uptake, freq) %>%
        slice(1)

      data.frame(
        weights_scenario = weight_name,
        policy_scenario = scenario_proj[m],
        gender = gender[i + 1],
        Strategy = topsis_index,
        topsis_strategy,
        setNames(topsis_metrics, metrics),
        check.names = FALSE
      )
    }) %>% bind_rows()
  }) %>% bind_rows()
}) %>% bind_rows()

# One row per weight/policy/gender combination. The six metric columns contain
# the normalized values of the strategy selected by TOPSIS.
topsis_metric_results = topsis_metric_results %>%
  select(
    weights_scenario, policy_scenario, gender, Strategy,
    min_age, max_age, uptake, freq,
    all_of(metrics)
  )

print(topsis_metric_results)
write.csv(
  topsis_metric_results,
  file = file.path(plot.dr, 'TOPSIS_normalized_metrics_by_weights.csv'),
  row.names = FALSE
)
