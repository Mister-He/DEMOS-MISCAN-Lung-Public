# dir='~/OneDrive - National University of Singapore/manus/lung_cancer/'
# setws(dir)
sim.dir = 'outs_sim/' #simulation output location
library(dplyr)
library(parallel)
library(grid)
library(viridis)
source('code/functions.R')

#all labels------------
gender = c('Male','Female'); n.gender = length(gender)
stage = paste0('Stage ', c('I','II','III','IV')); n.stage = length(stage)
# scenario_name = c('Baseline', 'Mild', 'Moderate', 'Stringent', 'Immediate Ban')
scenario_name = c('Status quo', 'Mild', 'Moderate', 'Stringent', 'Immediate ban'); n.scenario = length(scenario_name)
scenario_proj = c('Baseline', 'Mild (20%)', 'Moderate (50%)', 'High (80%)', 'Immediate Ban (100%)')
freq_name = c('Annual', 'Biennual', 'Triennial', 'Quadrennial', 'Quinquennial'); n.freq = length(freq_name)
freq_name_s = c('Ann', 'Bi','Tri','Quad','Quin')
uptakes = c(0.4, 0.7, 1)*100; n.uptake = length(uptakes)
metrics = c('QALY', 'LSA', 'Deaths_Averted', 'Cost_Savings','OD_rate','FP_rate'); n.metrics = length(metrics)
radar_metrics = paste0(metrics, '_scaled')
metric_name=c('QALY gains', 'Late-stage cancer averted', 'Deaths averted', 'Additional costs','Overdiagnosis rate','False-positive rate')
metric_name_short = c('QG', 'LSA', 'DA', 'AddCost', 'OD',  'FP')
# strategy_name = c('Most life-saving', 'Most cost-effective', 'Lowest-overdiagnosis', 'Overall optimal'); n.strategy = length(strategy_name)
# strategy_name1 = c('Most\nlife-saving', 'Most\ncost-effective', 'Lowest\noverdiagnosis', 'Overall\noptimal')
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


#output & observation data-----------
SI_dat = readRDS('outs/fig_s1_s2_s5_raw_data.rds')
cost_fit = read.csv('outs_sim/fig_s5.csv')
cost_obs = read.csv('outs_sim/fig_s5_obs.csv')

#per year + 5-year age group (35 to 85+) <--observed data is stored here
case=read.csv('outs/cancer_incidence_data_by_age_grp.csv')
#observed case data: 2009-2019
case_gender=case%>%
  group_by(year, gender)%>%
  summarise(inc=sum(Observed, na.rm=T), pop=sum(population, na.rm=T),.groups='keep')%>%
  arrange(year)
case_obs=lapply(tolower(gender), function(i){
  case_gender%>%filter(gender==i)%>%pull(inc)
})

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

#aggregated by period (08-12, 13-17, 18-22) and gender
# agg_fit = readRDS('outs/cancer_incidence_mortality_data_by_yrs.rds')
agg_fit = readRDS(paste0(sim.dir, ver, '/baseline/demos_lc_ind_death_with_ci_by_5_yrs_baseline.rds'))
# agg_yr=list(
#   c(2008, 2013, 2018, 2023), #incidence
#   c(2008, 2013, 2018) #death
# )

#smoking prevalence by year/gender/ethnicity
smoke=readRDS('outs/demos_smoke_prev_data.rds') #for observed data only
# smoke_ci=readRDS('outs/smoking_ci.rds') #100 simulations per group (11) per year (35)
# smoke_ci=smoke_ci[,-c(1:33)] #excluding historical data (pre-2019)
# smoke_summ=lapply(1:nrow(smoke$DEMOS), function(i){
#   n0=floor(ncol(smoke_ci)/nrow(smoke$DEMOS))
#   idx=i+(1:n0-1)*nrow(smoke$DEMOS)
#   smoke_ci[,idx]%>%estim
# })
# with tobacco control scenario, deriving mean and 95% UI from 100 simulations
smoke_ci = lapply(1:n.scenario, function(m){
  # name0 = ifelse(m==1, '', paste0('_', tolower(scenario_file[m])))
  # out = readRDS(paste0('outs/smoking_ci', name0,'.rds'))
  out = readRDS(paste0(sim.dir, ver, '/', tolower(scenario_file[m]), '/smoking_ci_', tolower(scenario_file[m]), '.rds'))
  out = out[,-c(1:33)] #excluding historical data (pre-2019)
  lapply(1:nrow(smoke$DEMOS), function(i){
    n0=floor(ncol(out)/nrow(smoke$DEMOS))
    idx=i+(1:n0-1)*nrow(smoke$DEMOS)
    out[,idx]%>%estim
  })
})%>%setNames(scenario_name)

#survival by stage
# s_stage=lapply(1:4, function(i){
#   #row: 2003-2019; col: year from diagnosis 0-18 (until 2021)
#   temp=read.csv(paste0('outs/survival_time_stage',i,'.csv'))
#   as.matrix(temp[,-1])
# })
# s_stage_raw=readRDS('outs/lung_cancer_survival_estimate_with_ci.rds')
s_stage_raw=read.csv('outs/lung_cancer_survival_estimate_with_ci.csv')

#projection by 2050 (screening strategies)
# proj = read.csv('outs/demos_screening_result_table.csv')
proj = proj_m = read.csv(paste0(sim.dir, ver, '/demos_screening_result_with_smoking_ban_policy_table_',
                       ifelse(ver == 'aggressive','with','without'),'_survival_extrapolation.csv'))

#sensitivity analysis: screening sensitivity
csv_files <- list.files(path = paste0(sim.dir, 'screen_sensitivity'), pattern = "\\.csv$", full.names = TRUE)
proj_sens = lapply(c(2,0,1), function(i){
  if(i==0){
    read.csv(paste0(sim.dir, 'aggressive/demos_screening_result_with_smoking_ban_policy_table_with_survival_extrapolation.csv'))
  }else{
    read.csv(csv_files[i])
  }
})

#sensitivity analysis: ppl with family history
csv_files <- list.files(path = paste0(sim.dir, 'fhlc'), pattern = "\\.csv$", full.names = TRUE)
idx = 1; ver = paste0('family_history',c('_l','')[ceiling(idx/2)])
ver0 = c( 'full', 'partial')[2-(idx-1)%%2]
proj = proj_fh = read.csv(csv_files[idx]); 
proj_sb = proj_clean(proj_fh, scale_all = F, new_derive = F, agg_by_seed = T, ever_smoker = F)
proj_sb0 = proj_clean(proj_fh, scale_all = F, new_derive = F, agg_by_seed = F, scale = F, ever_smoker = F)

#smoking packyear combinations
csv_files <- list.files(path = paste0(sim.dir, 'packyear'), pattern = "\\.csv$", full.names = TRUE)
idx = 1; proj = read.csv(csv_files[idx]); ver0 = ifelse(idx == 1, '_l', '')


#plots--------
plot.dr='plots/'

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


#smoking prev---------
smoking_sub=lapply(c(1,8,7), function(k){
  # smoke_summ[[k]]
  smoke_ci[[1]][[k]]
})
smoking_obs=lapply(c(1,8,7), function(k){
  mu=unlist(smoke$Observed$Mean[k,-1])
  sigma=unlist(smoke$Observed$Err[k,-1])
  cbind(mu, mu+qnorm(0.025, 0, sigma), mu+qnorm(0.975, 0, sigma))
})
line_name=c('Total', gender)
clr = with(color_list, c(tot_fit, male_fit, female_fit))
clr_1=with(color_list, c(tot_obs, male_obs, female_obs))
png(paste0(plot.dr,'smoking_fit.png'),width=12, height=8, units='cm', res=300,pointsize=10)
pushViewport(plotViewport(c(2,1,0,2)))
  xrange=c(2018.5,2050)
  yrange=c(0,25)
  margin=c(2,2.5,0.5,1)
  y_text=smoke_prev_name; x_text='Year'
  xtk=xlab=seq(2020,2050,5)
  plot_lines(smoking_sub,x_idx=2019:2050, col=clr)
  plot_linedots(smoking_obs, x_idx=2019:2024, col=clr_1)
  plot_xy()
  plot_legend(c(paste0(line_name,' (Modelled)'), paste0(line_name,' (Observed)')),
              col=c(clr,clr_1), topright = T, topright_pos = 8, n.pointlines = 3)
popViewport()
dev.off()

#survival by stage-------------
# years=c(2003, 2006, 2009, 2011)
years = 2008:2019
s_stage=lapply(0:1, function(smoking){
  lapply(years, function(y){
    lapply(1:n.stage, function(k){
      dat=s_stage_raw%>%filter(Stage==paste0('Stage',k))%>%
        #s_stage_raw[[paste0('Stage_', k)]]%>%
        filter(Year==y, Smoke==paste0('Smoke',smoking))%>%
        select(S_hat, S_low, S_high)%>%as.matrix
      dat*100
    })
  })
})
  
s_stage_obs=lapply(0:1, function(smoking){
  lapply(years, function(y){
    lapply(1:n.stage, function(k){
      dat=s_stage_raw%>%filter(Stage==paste0('Stage',k))%>%
        #s_stage_raw[[paste0('Stage_', k)]]%>%
        filter(Year==y, Smoke==paste0('Smoke',smoking))%>%
        # mutate(S_observed=S_observed*100)%>%
        # pull(S_observed)
        select(S_obs, S_obs_low, S_obs_high)%>%as.matrix()
      dat*100
    })
  })
})
  
clr= color_list$stage_fit
clr_1= color_list$stage_obs
line_name=stage
n.years=length(years); n.t = 15
n.col=4; n.row=ceiling(n.years/n.col * 2)
png(paste0(plot.dr,'surv_smoke.png'),width=n.col*6+4, height=5*n.row, units='cm', res=300,pointsize=10)
pushViewport(plotViewport(c(2,1,2,8.5)))
pushViewport(viewport(layout = grid.layout(nrow=n.row,ncol=n.col)))
for(s in 1:2){
  grid.text(c('Never-smoker', 'Ever-smoker')[s], x = 0.5 * s - 0.25, y = unit(1, 'npc') + unit(0.5, 'lines'), 
            gp = gpar(font = 2, cex = 2)) 
  for(k in 1:n.years){
    nrow=(k-1)%%n.row + 1; ncol=ceiling(k/n.row)+s*2-2
    pushViewport(viewport(layout.pos.row = nrow,layout.pos.col = ncol))
    xrange=c(0, n.t+0.5); yrange=c(0,110)
    xtk=xlab=seq(0, n.t, 5)
    margin=c(2,2.5,0.5,1)
    y_text='Survival (%)'; x_text='Time from diagnosis (yr)'
    plot_lines(s_stage[[s]][[k]], col=clr)
    # plot_linedots(s_stage[[k]], x_idx=0:20-0.1, col=clr, point_style = 15:18, 
    #               cex=0.4, linkpoints = T, line_wd = 1.5, link_lty = 1)
    for(m in 1:n.stage){
      plot_linedots(list(s_stage_obs[[s]][[k]][[m]]), x_idx=c(0,1:n.t+c(-1,1)[(m%%2)+1]*0.1), col=clr_1[m], point_style = 14+m, 
                    cex=0.4, line_alpha = 0.3, point_alpha = 0.5, line_wd=1.5)
    }
    
    plot_xy(xmargin = (nrow == n.row), ymargin = (ncol==1), 
            plot_label = paste0('(', letters[k + n.years * (s-1)],') ', years[k]))
    popViewport()
  }
}

popViewport()
plot_legend(c(paste0(line_name,' (Modelled)'), paste0(line_name,' (Observed)')),
            col=c(clr, clr_1), n.points = 4, point_style = 15:18, cex = 0.6,
            topright = F)
popViewport()
dev.off()


#aggregated incidence and death------
plot_name=inc_death_name
clr = with(color_list, c(male_fit, female_fit))
clr_1 = with(color_list, c(male_obs, female_obs))
line_name=gender
fit_stat=lapply(plot_name, function(k){
  lapply(tolower(line_name), function(i){
    dat=agg_fit[[k]]%>%
      filter(gender==i)%>%
      #mutate(DEMOS = DEMOS /1e3)%>%
      select(all_of(paste0('DEMOS', c('', '_low', '_high'))))%>%
      as.matrix()
    dat/1e3
  })
})
obs_stat=lapply(plot_name, function(k){
  lapply(tolower(line_name), function(i){
    agg_fit[[k]]%>%filter(gender==i)%>%
      mutate(Observed = Observed /1e3)%>%
      pull(Observed)
  })
})
periods=lapply(plot_name, function(k){
  agg_fit[[k]]%>%filter(gender=='male')%>%pull(period)
})
ylim=c(10, 8)
png(paste0(plot.dr,'agg_inc.png'),width=20, height=7, units='cm', res=300,pointsize=10)
pushViewport(plotViewport(c(1,2,0,8.5)))
pushViewport(viewport(layout = grid.layout(nrow=1,ncol=2)))
for(k in 1:2){
  pushViewport(viewport(layout.pos.row = 1,layout.pos.col = k))
  xrange=c(0, length(periods[[k]]))+0.5; yrange=c(0,ylim[k])
  xlab=periods[[k]]; xtk=c(0:length(periods[[k]]))+0.5; xtk1=seq_along(periods[[k]])
  margin=c(2,1.5,0.5,1)
  y_text='Count (thousand)'; x_text='Time period (yr)'
  n.period = length(periods[[k]])
  for(i in 1:2){
    plot_linedots(list(fit_stat[[k]][[i]]), x_idx=1:n.period+c(-1,1)[i]*0.01*n.period, col=clr[i], 
                  linkpoints = T, line_wd = 1.5, point_alpha = 0.6, line_alpha = 0.5)
    plot_linedots(list(obs_stat[[k]][[i]]), x_idx=1:n.period+c(-1,1)[i]*0.01*n.period, col=clr_1[i], 
                  point_style = 4,  ci=F, point_alpha = 1, cex = 0.6)
  }
  # plot_linedots(fit_stat[[k]], x_idx=1:length(periods[[k]])-0.1, col=clr, linkpoints = T)
  # plot_linedots(obs_stat[[k]], x_idx=1:length(periods[[k]])+0.1, col=clr_1, linkpoints = T, ci=F)
  plot_xy(xmargin = T, ymargin = (k==1), closer_y = 0.3, customize_xaxis = T, closer_x=1, 
          plot_label = paste0('(', letters[k],') ', plot_name[k]))
  if(k==2) plot_legend(c(paste0(line_name,' (Modelled)'), paste0(line_name,' (Observed)')),
                       col=c(clr, clr_1), n.pointlines =2, n.points=2, point_style = c(19,19,4, 4),
                       topright = F)
  popViewport()
}
popViewport()
popViewport()
dev.off()

#annual incidence and death projection until 2050 (baseline)----------
proj_stat=list(
  outcomes[[1]]$inc_proj[-1],
  outcomes[[1]]$death_proj[-1]
)
obs_stat=list(lapply(case_obs, function(k) k/1e3))
plot_name=inc_death_name
clr = with(color_list, c(male_fit, female_fit))
clr_1 = with(color_list, c(male_obs, female_obs))
line_name= gender
ylim=c(3, 2.5)
png(paste0(plot.dr,'yr_inc_',ver,'.png'),width=20, height=7, units='cm', res=300,pointsize=10)
pushViewport(plotViewport(c(1,2,0,8.5)))
pushViewport(viewport(layout = grid.layout(nrow=1,ncol=2)))
for(k in 1:2){
  pushViewport(viewport(layout.pos.row = 1,layout.pos.col = k))
  xrange=c(2008,2050.5); yrange=c(0, ylim[k])
  xlab=xtk=seq(2010,2050,10)
  margin=c(2,2,0.5,1)
  y_text='Count (thousand)'; x_text='Year'
  plot_lines(proj_stat[[k]], col=clr)
  if(k==1) plot_linedots(obs_stat[[k]], col=clr_1, ci=F, cex=0.3)
  plot_xy(xmargin = T, ymargin = (k==1), closer_y = 0.3,
          plot_label = paste0('(', letters[k],') ', plot_name[k]))
  if(k==2) plot_legend(c(paste0(line_name,' (Modelled)'), paste0(line_name,' (Observed)')),
                       col=c(clr, clr_1), n.points = 2,
                       topright = F)
  popViewport()
}
popViewport()
popViewport()
dev.off()

#fit and projection combined plot-----------
plot_data = list(
  #for all: total, male, female
  #1st row: smoking prevalence
  lapply(c(1, 8, 7), function(k){
    proj = lapply(1:n.scenario, function(m){
      if(m==1){
        smoke_ci[[m]][[k]] #2019-2050
      }else{
        smoke_ci[[m]][[k]][-c(1:6), ] #2025-2050
      }
    })
    #observed historical data
    mu=unlist(smoke$Observed$Mean[k,-1])
    sigma=unlist(smoke$Observed$Err[k,-1])
    obs=cbind(mu, mu+qnorm(0.025, 0, sigma), mu+qnorm(0.975, 0, sigma))
   list(proj = proj, obs = obs)
  }),
  #2nd row: incidence
  lapply(1:3, function(k){
    proj = lapply(1:n.scenario, function(m){
      if(m==1){
        outcomes[[m]]$inc_proj[[k]] #2009-2050
      }else{
        outcomes[[m]]$inc_proj[[k]][-c(1:17), ] #2025-2050
      }
    })
    if(k==1){
      obs = (case_obs[[1]] + case_obs[[2]])/1e3
    }else{
      obs = case_obs[[k-1]]/1e3
    }
    list(proj = proj, obs = obs)
  }),
  
  #3rd row: death
  lapply(1:3, function(k){
    proj = lapply(1:n.scenario, function(m){
      if(m==1){
        outcomes[[m]]$death_proj[[k]] #2009-2050
      }else{
        outcomes[[m]]$death_proj[[k]][-c(1:17), ] #2025-2050
      }
    })
    list(proj = proj, obs = NULL)
  })
)
x_idx = list(
  list(proj = 2019:2050, obs = 2019:2024), 
  list(proj = 2008:2050, obs = 2008:2018), 
  list(proj = 2008:2050)
)
clr = color_list$scenario
clr_1 = color_list$single_obs
nrow = length(plot_data); ncol = length(plot_data[[1]])
y_text_list = c(smoke_prev_name, paste0(inc_death_name, ' (000s)'))
y_lim = c(25, 5, 3.2); ytk = ytk1 = ylab = 0:3
png(paste0(plot.dr,'fit+proj_',ver,'.png'),width=5+6*ncol, height=6*ncol+1, units='cm', res=300,pointsize=10)
pushViewport(plotViewport(c(1,1.5,1,6.5)))
pushViewport(viewport(layout = grid.layout(nrow=nrow,ncol=ncol)))
for(i in 1:nrow){
  for(j in 1:ncol){
    pushViewport(viewport(layout.pos.row = i,layout.pos.col = j))
    margin=c(3,2,0.5,1)
    xrange=c(min(x_idx[[i]]$proj)-0.5,2050.5); yrange=c(0, y_lim[i])
    xlab=xtk=seq(ceiling(xrange[1]/10)*10, 2050, 10)
    y_text=y_text_list[i]; x_text='Year'
    dat1 = plot_data[[i]][[j]]
    if(length(dat1$proj)>1){
      plot_lines(dat1$proj[-1], col=clr[-1], x_idx = 2025:2050, ci_alpha = 0.15)
    }
    plot_lines(list(dat1$proj[[1]]), col=clr[1], x_idx = x_idx[[i]]$proj)
    if(!is.null(dat1$obs)){
      plot_linedots(list(dat1$obs), x_idx = x_idx[[i]]$obs, col=clr_1, ci=(i==1), cex=0.3)
    }
    plot_xy(xmargin = (i==nrow), ymargin = (j==1), closer_y = 0.2, 
            x_name = ifelse(i ==1, c('Total', gender)[j], NA),
            customize_yaxis = (i==3), lab_pos_y = 0.75, 
            plot_label = paste0('(', letters[(i-1)*nrow+j],') '), v_ref = 2025)
    popViewport()
  }
}
popViewport()
plot_legend(c(scenario_name, 'Observation'), col=c(clr, clr_1), n.points = 1, topright = F)
popViewport()
dev.off()


#heatmap of the screening strategies (baseline scenario)--------
proj_sb = proj_clean(proj, scale_all = F, new_derive = T, agg_by_seed = T)
# proj_sb = proj_clean(proj, scale_all = F, new_derive = F, agg_by_seed = T)
s = 1
#for(s in 1:n.scenario)
{
  scenario0 = scenario_proj[s]
  proj_dat=lapply(0:1, function(i){ #by gender
    dat1 = lapply(1:n.freq, function(k){ #by frequency
      proj_sb%>%
        filter(screen_gender == i, freq == k, Smoking_ban_policy == scenario0)%>%
        arrange(uptake, min_age, max_age)
    })
    outcome = lapply(1:n.freq, function(k){
      dat1[[k]]%>%
        select(all_of(radar_metrics))%>%
        as.matrix
    })
    strategy = dat1[[2]]%>%
      mutate(cat = paste0(min_age, '-', max_age))%>%
      pull(cat)
    list(outcome = outcome, strategy = strategy)
  })
  clr = color_list$metrics
  png(paste0(plot.dr,'screen_heatmap_',ver,'_',scenario_name[s],'.png'),width=16, height=20, units='cm', res=300,pointsize=10)
  pushViewport(plotViewport(c(0,3,1,11)))
  pushViewport(viewport(layout = grid.layout(nrow=1,ncol=2)))
  for(k in 1:2){
    pushViewport(viewport(layout.pos.row = 1,layout.pos.col = k))
    n.cats = n.metrics
    n.rows = nrow(proj_dat[[k]]$outcome[[2]])
    xrange = c(0, n.cats); yrange = c(0, n.rows)
    #xtk=0:n.cats; xlab = rep('', n.cats+1)
    margin=c(1,1,0.5,0.5)
    y_text=''; x_text=''
    if(k==1) {
      add_lab = paste0(c(40, 70, 100)%>%rev, '% Uptake')
    }else{
      add_lab = NA
    }
    plot_rect(proj_dat[[k]]$outcome, row_name = proj_dat[[k]]$strategy, col_sub =n.freq, 
              row_name2 = add_lab, line_wd = 1.5, rearrange = (ver=='family_history'),
              ytk2 = 0:3 * (n.rows/3), col_name = paste0('(', letters[k], ') ', gender[k]),
              col = clr, shade_alpha = 0.8, label = (k==1))
    #plot_xy(xmargin = T, ymargin = F, plot_y = F, plot_x = F, x_name = gender[k] )
    popViewport()
  }
  popViewport()
  plot_legend(metric_name, col=clr, n.onlyshade = n.metrics, topright = F, shade_alpha = 0.8)
  popViewport()
  dev.off()
}

#radar plot-------------
# table(proj$Smoking_ban_policy, useNA = 'always')
radar_idx = c(1, 4, 3, 5, 2, 6)
# scenario_proj = c('Baseline', 'Mild (20%)', 'Moderate (50%)', 'High (80%)', 'Immediate Ban (100%)')
# proj_sb = proj_clean(proj, scale_all = F, new_derive = T, agg_by_seed = T)
proj_sb0 = proj_clean(proj, scale_all = F, new_derive = T, agg_by_seed = F, scale = F)
n.sim = max(proj_sb0$Seed)
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
      # is_front = pareto_frontier(dat2 %>% filter(OD_rate <= 10), cols = radar_metrics, maximize = rep(T, n.metrics))
      # is_front = pareto_frontier(dat2, cols = radar_metrics, maximize = rep(T, n.metrics))
      scores = compute_topsis_scores(
        df            = dat2, #%>% 
          # filter(OD_rate <= 10) %>% 
          # slice(is_front),
        # weights = c(1, 0.5, 0.5, 1, 1, 1),
        # weights = c(1, 1, 1, rep(1.5, 2), 1),
        decision_cols = radar_metrics,
        beneficial    = rep(TRUE, n.metrics)
      )
      scores
    }, mc.cores = detectCores())%>%do.call('rbind',.)%>%as.matrix#%>%
    #colMeans()%>%which.max
    # topsis = eligible[topsis_idx]
    topsis = eligible[which.max(colMeans(topsis_idx))]
    # topsis = eligible[which.max(larger_prob(topsis_idx, summ = T))]
    # topsis_rank = lapply(1:n.sim, function(s){
    #   # rank(-topsis_idx[s,])==1
    #   rank(topsis_idx[s,])
    # })%>%do.call('rbind',.)%>%as.matrix
    # topsis0 = which(colMeans(topsis_rank)==max(colMeans(topsis_rank)))
    # topsis = eligible[topsis0[which.max(colMeans(topsis_idx)[topsis0])]]
    # is_front = pareto_frontier(dat2 %>% filter(OD_rate <= 10), cols = radar_metrics, maximize = rep(T, n.metrics))
    # scores = compute_topsis_scores(
    #   df            = dat1 %>% filter(OD_rate <= 10) %>% slice(is_front),
    #   decision_cols = radar_metrics,
    #   beneficial    = rep(TRUE, n.metrics)
    # )
    # topsis = dat1 %>% filter(OD_rate <= 10) %>% slice(is_front) %>% slice(which.max(scores)) %>% pull(Strategy)
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
# clr = color_list$strategy
# png(paste0(plot.dr,'screen_radar_',ver,'.png'),width=16, height=4.5*n.scenario, units='cm', res=300,pointsize=10)
# pushViewport(plotViewport(c(1,2,2,11.5)))
# pushViewport(viewport(layout = grid.layout(nrow=n.scenario,ncol=2)))
# for(m in 1:n.scenario){
#   for(k in 1:2){
#     margin=c(1, 0.5, 0, 2)
#     pushViewport(viewport(layout.pos.row = m,layout.pos.col = k))
#     plot_radar(radar_dat[[m]][[k]]$metrics[,radar_idx], col=clr, size = 0.85, 
#                shade = c(rep(F, n.strategy-1), T), shade_alpha = 0.1,
#                line_alpha = c(rep(0.6, n.strategy-1), 0.8),
#                line_type = c(2:n.strategy, 1),
#                label = metric_name_short[radar_idx], plot_name = paste0('(',letters[k+(m-1)*2], ')'))
#     if(m==1){
#       plot_xy(xmargin = F, ymargin = F, 
#               x_name = gender[k], x_name_pos = 0.5, 
#               plot_x = F, plot_y = F)
#     }
#     if(k==1){
#       plot_xy(xmargin = F, ymargin = F, 
#               y_name = scenario_name[m], y_name_pos = 1, rot_y = T,
#               plot_x = F, plot_y = F)
#     }
#     popViewport()
#   }
# }
# popViewport() 
# plot_legend(strategy_name, col=clr, topright = F, right_pos = 2, line_length = 1.5,
#             n.lines = n.metrics, line_type = c(2:n.strategy, 1))
# popViewport()
# dev.off()

#selected strategy-----------
# selected_strategy = lapply(1:n.strategy, function(i){
#   lapply(1:2, function(k){
#     lapply(1:n.scenario, function(m){
#       radar_dat[[m]][[k]]$strategy%>%
#         mutate(gender = k-1, scenario = scenario_name[m])%>%
#         select(gender, scenario, min_age, max_age, uptake, freq)%>%slice(i)
#     })%>%do.call('rbind',.)%>%as.data.frame()
#   })%>%do.call('rbind',.)%>%as.data.frame()%>% 
#     arrange(freq, uptake, min_age, max_age)%>%
#     mutate(name = paste0(freq_name[freq], ', ', min_age, '-', max_age, ', ', paste0(uptake * 100, '% uptake')))%>%
#     group_by(name)%>%
#     group_split
# })%>%setNames(strategy_name)
# # saveRDS(selected_strategy, 'outs/strategy.rds')
# 
# # clr = as.vector(color_list$scenario_g)
# clr = color_list$scenario_g
# png(paste0(plot.dr,'strategies_',ver, '.png'),width=16, height=16, units='cm', res=300,pointsize=10)
# pushViewport(plotViewport(c(1,0.5,1,1)))
# margin = c(1, 13.5, 0, 1)
# plot_strategy2(selected_strategy, col = clr)
# # margin = c(0,0,0,10)
# # plot_strategy(selected_strategy, point_style = 15:16, col =  clr, point_size = 1)
# # plot_legend(c(paste0(scenario_name, ' (Male)'), paste0(scenario_name, ' (Female)')),
# #             n.points = n.scenario*2, point_style = rep(15:16, each = n.scenario), 
# #             col = clr, cex = 1)
# popViewport()
# dev.off()


#radar plot & strategy for baseline------------
m = 1 # baseline
clr = color_list$strategy.g
name_all = lapply(gender, function(i) paste0(strategy_name, ' (', i, ')'))%>%unlist
png(paste0(plot.dr,'select_',scenario_file[m],'_',ver,'.png'),width=25, height=12, units='cm', res=300,pointsize=10)
pushViewport(plotViewport(c(1,1,1,15)))
pushViewport(viewport(layout = grid.layout(nrow=3,ncol=n.strategy)))
for(k in 1:2){
  for(s in 1:n.strategy){
    margin=c(1, 1, 1, 2.5)
    pushViewport(viewport(layout.pos.row = k,layout.pos.col = s))
    plot_radar(radar_dat[[m]][[k]]$metrics[s,radar_idx], col=clr[s,k], size = 0.85, 
               shade = T, shade_alpha = 0.2,
               line_alpha = 0.8, line_type = 1, plot_name_bold = F,
               label = metric_name_short[radar_idx], plot_name = paste0('(',letters[s+(k-1)*n.strategy], ')'))
    if(s==1){
      plot_xy(xmargin = F, ymargin = F, 
              y_name = gender[k], y_name_pos = 1, rot_y = T,
              plot_x = F, plot_y = F)
    }
    # if(k==1){
    #   plot_xy(xmargin = F, ymargin = F, 
    #           x_name = strategy_name[s], x_name_pos = 0.5, 
    #           plot_x = F, plot_y = F)
    # }
    popViewport()
  }
}
for(s in 1:n.screen_vars){
  ylab = range_list[[screen_vars[s]]]; ytk = ytk1 = seq_along(ylab)
  xtk = 0:n.strategy; xtk1 = 1: n.strategy - 0.5; xlab = strategy_name1
  # xtk = xtk1 = 1:n.strategy - 0.5; xlab= rep('',n.screen_vars)
  yrange = c(0, length(ylab) ); xrange = c(0, n.strategy)
  margin=c(0.5, 2, 1.5, 1)
  pushViewport(viewport(layout.pos.row = 3,layout.pos.col = s))
  plot_data = lapply(1:2, function(k){
    x = radar_dat[[m]][[k]]$strategy[[screen_vars[s]]]
    x1 = lapply(x, function(i){
      if(screen_vars[s] == 'uptake'){
        which((ylab/100) == i)
      }else if(screen_vars[s] == 'freq'){
        #length(ylab) + 1 - i
        i
      }else{
        which(ylab == i)
      }
    })%>%unlist
  })
  plot_bar(plot_data, col = clr, width_between = 0.4)
  y_text = lab_size_y = ifelse(screen_vars[s] == 'freq', '', screen_varname[s]); x_text = ''
  plot_xy(xmargin = F, ymargin = T, 
          plot_label = paste0('(',letters[n.strategy*2 + s],')'),
          closer_y = 0.6, plot_label_above = T, lab_size_x = 0.35, lab_pos_x = 0.8,
          customize_yaxis = T, customize_xaxis = T,lab_pos_y = 0.5,
          lab_size_y = ifelse(screen_vars[s] == 'freq', 0.6, 1))
  popViewport()
}
popViewport() 
plot_legend(name_all, col=as.vector(clr), topright = F, right_pos = 2, line_length = 1.5,
            n.lines = n.strategy * 2)
popViewport()
dev.off()


#strategies for all five scenarios----------
clr = color_list$strategy.g
name_all = lapply(gender, function(i) paste0(strategy_name, ' (', i, ')'))%>%unlist
png(paste0(plot.dr,'select_all_',ver,'.png'),width=31, height=18, units='cm', res=300,pointsize=10)
pushViewport(plotViewport(c(0,2,2,15)))
pushViewport(viewport(layout = grid.layout(nrow=4,ncol=1)))
for(k in 1:2){
  grid.text(gender[k], unit(-0.5, 'lines'), 1.25 - k/2, gp=gpar(font=2, cex = 1.5), rot = 90)
  #radar plot
  margin=c(1, 0.5, 0.5, 2)
  pushViewport(viewport(layout.pos.row = k*2 - 1,layout.pos.col = 1))
  pushViewport(viewport(layout = grid.layout(nrow=1,ncol=n.scenario)))
  for(m in 1:n.scenario){
    pushViewport(viewport(layout.pos.row = 1,layout.pos.col = m))
    plot_radar(radar_dat[[m]][[k]]$metrics[,radar_idx], col=clr[,k], size = 0.85, 
               shade = c(rep(F, n.strategy-1), T), shade_alpha = 0.1,
               line_alpha = c(rep(0.6, n.strategy-1), 0.8),
               line_type = c(2:n.strategy, 1), 
               label = metric_name_short[radar_idx], plot_name_bold = F,
               plot_name = paste0('(',letters[(k-1) * (n.scenario + n.screen_vars)+m], ') ', scenario_name[m]))
    popViewport()
  }
  popViewport()
  popViewport()
  pushViewport(viewport(layout.pos.row = k*2,layout.pos.col = 1))
  pushViewport(viewport(layout = grid.layout(nrow=1,ncol=n.screen_vars)))
  for(s in 1:n.screen_vars){
    ylab = range_list[[screen_vars[s]]]; ytk = ytk1 = seq_along(ylab)
    xtk = 0:n.strategy; xtk1 = 1: n.strategy - 0.5; xlab = rep('',n.strategy); xlab = strategy_name1
    # xtk = xtk1 = 1:n.strategy - 0.5; xlab= rep('',n.screen_vars)
    yrange = c(0, length(ylab) ); xrange = c(0, n.strategy)
    margin=c(3, 2, 2, 1)
    pushViewport(viewport(layout.pos.row = 1,layout.pos.col = s))
    plot_data = lapply(1:n.scenario, function(m){
      x = radar_dat[[m]][[k]]$strategy[[screen_vars[s]]]
      x1 = lapply(x, function(i){
        if(screen_vars[s] == 'uptake'){
          which((ylab/100) == i)
        }else if(screen_vars[s] == 'freq'){
          # length(ylab) + 1 - i
          i
        }else{
          which(ylab == i)
        }
      })%>%unlist
    })
    plot_bar(plot_data, col = clr[,k], width_between = 0.3, same_col = T)
    y_text = ''; x_text = ''
    plot_xy(xmargin = F, ymargin = T, 
            plot_label = paste0('(',letters[(k-1) * (n.scenario + n.screen_vars)+s + n.scenario],') ', screen_varname[s]),
            closer_y = 0.6, plot_label_above = T, lab_size_x = 0.5, lab_pos_x = 0.8,
            customize_yaxis = T, customize_xaxis = T,lab_pos_y = 0.5, lab_size_y = ifelse(screen_vars[s] == 'freq', 0.6, 1))
    popViewport()
  }
  popViewport()
  popViewport()
}
popViewport() 
plot_legend(name_all, col=clr%>%as.vector(), topright = F, right_pos = 2, line_length = 1.5,
            n.lines = n.metrics * 2, line_type = rep(c(2:n.strategy, 1),2))
popViewport()
dev.off()


#scatter plot of cost, QALY, ICER-------------
ver0 = c('raw', 'full', 'partial')[1]; ver1 = c('median', 'all')[2]
if(ver0 == 'raw'){
  vars = c('QALY_raw', 'Cost', 'ICER'); n.vars = length(vars)
  var_name = list('QALY (million)', 'Cost (billion SGD)', 'ICER  (thousand SGD/QALY)')
}else{
  vars = c('QALY', 'Cost_Savings', 'ICER'); n.vars = length(vars)
  var_name = list('QALY gains (thousand)', 'Additional costs (million SGD)', 'ICER  (thousand SGD/QALY)')
}

proj_dat = lapply(vars, function(var0){
  lapply(1:n.freq, function(i){
    lapply(0:1, function(j){
      lapply(1:n.scenario, function(k){
        if(ver1 == 'median'){
          dat0 = proj_sb
        }else{
          dat0 = proj_sb0
        }
        dat0%>%
          filter(freq == i, screen_gender == j, Smoking_ban_policy == scenario_proj[k])%>%
          mutate(QALY = QALY /1e3, ICER = ICER/1e3, Cost_Savings = Cost_Savings/1e6,
                 QALY_raw = QALY_raw/1e6, Cost = Cost/1e9
                 )%>%
          # slice(1:(25*108)*2)%>%
          pull(var0)
      })
    })
  })
})

if(ver0 == 'raw'){
  base_ref = proj%>%
    filter(screen == 0, Smoking_ban_policy == 'Baseline')%>%
    rename(QALY_raw = !!as.symbol(ifelse('QALY_raw'%in%names(proj), 'QALY_raw', 'QALY')))%>%
    mutate(QALY_raw = QALY_raw/1e6, Cost = Cost/1e9)%>%
    select(QALY_raw, Cost)%>%
    summarise(QALY_raw = median(QALY_raw), 
              Cost = median(Cost))
}else{
  base_ref = data.frame(ICER = 120)
}

if(ver == 'family_history'){
  #no annual screening
  clr = cbind(
    viridis(1+n.freq, option = 'G')[1:n.freq + 1]%>%rev, #male
    viridis(2+n.freq, option = 'F')[1:n.freq + 2]%>%rev #female
  )
}else{
  clr = color_list$freq_g
}

if(ver == 'conservative') { #only median estimates
  xlim_list = list(c(85.6, 85.76), c(7.2, 10.5), c(30,67))
  xtk_list = list(seq(85.6, 85.75, 0.03), 8:10, 3:6*10)
}else if(ver == 'aggressive'){
  if(ver1 == 'median'){
    if(ver0 == 'raw'){ #original scale
      xlim_list = list(c(85.7, 85.83), c(9, 12.5), c(25,65))
      xtk_list = list(seq(85.7, 85.82, 0.03), 9:12, 3:6*10)
      plot_name = 'm_raw'
    }else{ #relative scale
      plot_name = 'm_rel'
    }
  }else{ # all estimates
    if(ver0 == 'raw'){ #original scale
      xlim_list = list(c(85.3, 86.1), c(6, 19), c(15,90))
      xtk_list = list(seq(85.4, 86.1, 0.2), seq(7,19,3), 1:4*20)
      plot_name = 'a_raw'
    }else{
      #relative scale
      xlim_list = list(c(0, 80), c(0,3)*1e3, c(15,90))
      xtk_list = list(0:4*20, 0:3*1e3, 1:4*20)
    }
  }
}else if(ver == 'family_history'){
  if(ver1 == 'median'){
    if(ver0 == 'raw'){ #original scale
      xlim_list = list(c(85.7, 85.82), c(9, 11.3), c(25,90))
      xtk_list = list(seq(85.7, 85.82, 0.03), 9:11, seq(30,90,30))
      
      plot_name = 'm_raw'
    }else if(ver0 == 'full'){ #relative scale, compared to no-screening
      xlim_list = list(c(0, 32), c(0,1.1)*1e3, c(20,100))
      xtk_list = list(0:3*10, 0:3*300, 2:5*20)
      plot_name = 'm_rel'
    }else if(ver0 == 'partial'){ #relative scale, compared to screening ever-smoker only
      xlim_list = list(c(0, 2.4), c(0, 350), c(0,350))
      xtk_list = list(seq(0, 2, 0.5), 0:3*100, seq(0,300,100))
      plot_name = 'm_scr'
    }
  }else{ # all estimates
    if(ver0 == 'raw'){ #original scale
      xlim_list = list(c(85.3, 86.1), c(6, 19), c(15,90))
      xtk_list = list(seq(85.4, 86.1, 0.2), seq(7,19,3), 1:4*20)
      plot_name = 'a_raw'
    }else if(ver0 == 'full'){
      #relative scale
      xlim_list = list(c(0, 48), c(0,1.3)*1e3, c(0,150))
      xtk_list = list(0:4*10, 0:4*300, 0:5*30)
      plot_name = 'a_rel'
    }else if(ver0 == 'partial'){
      xlim_list = list(c(0, 4.5), c(0,360), c(0,600))
      xtk_list = list(0:4, 0:3*100, seq(0,600,200))
      plot_name = 'a_scr'
    }
  }
}else if(ver == 'family_history_l'){
  if(ver1 == 'median'){
    if(ver0 == 'raw'){ #original scale
      xlim_list = list(c(85.7, 85.83), c(9, 12.7), c(40,138))
      xtk_list = list(seq(85.7, 85.82, 0.03), 9:12, 2:6*20)
      
      plot_name = 'm_raw'
    }else if(ver0 == 'full'){ #relative scale, compared to no-screening
      xlim_list = list(c(0, 50), c(0,2.6)*1e3, c(40,138))
      xtk_list = list(0:5*10, 0:5*500, 2:6*20)
      plot_name = 'm_rel'
    }else if(ver0 == 'partial'){ #relative scale, compared to screening ever-smoker only
      xlim_list = list(c(0, 3), c(0, 650), c(0,500))
      xtk_list = list(0:3, 0:3*200, seq(0,500,100))
      plot_name = 'm_scr'
    }
  }else{ # all estimates
    if(ver0 == 'raw'){ #original scale
      xlim_list = list(c(85.2, 86.1), c(6, 19), c(0,205))
      xtk_list = list(seq(85.2, 86, 0.2), seq(6,18,3), 0:4*50)
      plot_name = 'a_raw'
    }else if(ver0 == 'full'){
      #relative scale
      xlim_list = list(c(0, 72), c(0,3)*1e3, c(0,205))
      xtk_list = list(0:3*20, 0:3*1e3, 0:4*50)
      plot_name = 'a_rel'
    }else if(ver0 == 'partial'){
      xlim_list = list(c(0, 5.5), c(0,700), c(0,700))
      xtk_list = list(0:5, 0:3*200, seq(0,600,200))
      plot_name = 'a_scr'
    }
  }
}

point_list = list(c(15:18,4), c(15:18,4))
png(paste0(plot.dr,'scatter_CEA_',ver,'_', plot_name,'.png'),width=16, height=n.vars*6+2, units='cm', res=300,pointsize=10)
pushViewport(plotViewport(c(0,5,0.5,10)))
pushViewport(viewport(layout = grid.layout(nrow=n.vars,ncol=1)))
for(k in 1:n.vars){
  pushViewport(viewport(layout.pos.row = k,layout.pos.col = 1))
  yrange = c(0, n.scenario) + 0.5; ytk = 0:n.scenario + 0.5; ytk1 = n.scenario:1; ylab = scenario_name
  xrange = xlim_list[[k]]; xtk1 = xtk = xlab = xtk_list[[k]]
  margin=c(2,0,1,1)
  y_text=''; x_text = ''
  {
    for(j in 1:2){ #gender
      for(m in 1:n.freq){
        for(n in 1:n.scenario){
          y = proj_dat[[k]][[m]][[j]][[n]]
          if(length(y)>0){
            if(ver1 == 'median'){
              #one line
              y.pos = n.scenario + 1 - n
              s_para = 0.3
              p_alpha = 0.8; p_size = 0.4
            }else{
              #two lines
              y.pos = n.scenario + 1 - n + (j-1.5)*0.4
              s_para = 0.15
              p_alpha = (n.freq - m) * 0.05+0.05; p_size = 0.1
            }
            plot_linedots(list(y), x_idx = rep(y.pos, length(y)), ci = F, col = clr[m,j], 
                          scatter = T, point_alpha = p_alpha, scatter_para = s_para,
                          value_at_y = F, point_style = point_list[[j]][m], cex = p_size)
          }
         
        }
      }
    }
  }
  if(vars[k]%in%names(base_ref)){
    v_ref = base_ref[[vars[k]]][1]
    if(v_ref > xrange[2]) v_ref = NA
  }else{
    v_ref = NA
  }
  plot_xy(xmargin = T, ymargin = T, plot_y = T, plot_x = T,  closer_x = 0.5, 
          v_ref = v_ref, ref_col = 'grey70',
          customize_yaxis = T, customize_xaxis = T, lab_size_y = 0.8, lab_pos_x = 1,
          plot_label = paste0('(', letters[k],') ', var_name[k]), plot_label_above = 1)
  popViewport()
}
popViewport()
if(ver%in%c('family_history')){
  plot_legend(lapply(1:2, function(i) paste0(freq_name[-1], ' (', gender[i], ')'))%>%unlist, 
              col=clr[-1,]%>%as.vector(), n.points = (n.freq-1)*2, topright = F, 
              point_style = unlist(point_list)[-c(1,n.freq+1)], cex = 0.5)
}else{
  plot_legend(lapply(1:2, function(i) paste0(freq_name, ' (', gender[i], ')'))%>%unlist, 
              col=clr%>%as.vector(), n.points = n.freq*2, topright = F, 
              point_style = unlist(point_list), cex = 0.5)
}
popViewport()
dev.off()

#for OD and FP
# vars = c('Late.cases.diagnosed','Total.deaths','OverDiagnosis', 'False.Positives'); n.vars = length(vars)
# proj_dat = lapply(vars, function(var0){
#   lapply(1:n.freq, function(i){
#     lapply(0:1, function(j){
#       lapply(1:n.scenario, function(k){
#         proj_sb%>%
#           filter(freq == i, screen_gender == j, Smoking_ban_policy == scenario_proj[k])%>%
#           mutate(across(all_of(vars[1:2]), function(x) x/1e4), 
#                  OverDiagnosis = OverDiagnosis/1e3, False.Positives = False.Positives/1e5)%>%
#           # mutate(QALY = QALY /1e3, ICER = ICER/1e3, Cost_Savings = Cost_Savings/1e6,
#           #        QALY_raw = QALY_raw/1e6, Cost = Cost/1e9)%>%
#           pull(var0)
#       })
#     })
#   })
# })

# xlim_list = list(c(10.5,14), c(5.2, 7.3), c(0,5), c(0, 8))
# xtk_list = list(11:14, seq(5.5, 7, 0.5), 0:5, 0:4*2)
# var_name = list('Last-stage cases (0 000s)', 'Total deaths (0 000s)',
#                 'Overdiagnosis (000s)', 'False positives (00 000s)')
# base_ref = proj%>%
#   filter(screen == 0, Smoking_ban_policy == 'Baseline')%>%
#   mutate(across(all_of(vars[1:2]), function(x) x/1e4))%>%
#   # select(all_of(vars[1:2]))%>%
#   summarise(Late.cases.diagnosed = median(Late.cases.diagnosed), 
#             Total.deaths = median(Total.deaths))

#scatter plot of ratio with the recommended strategy (one-way analysis)----------
ver1 = 1
#MLSOD10 as reference
if(ver1 == 1){
  base_stra = data.frame(min_age = 50, max_age = 80, freq = 1, uptake = 1)
  alter_val = list(
    ages = cbind(
      c(50, 50, 55, 60, 55),
      c(75, 85, 80, 80, 85)
    ), 
    uptake = c(0.4, 0.7),
    freq = 2:5
  )
}
#TOPSIS as reference
if(ver1 == 2){
  base_stra = data.frame(min_age = 60, max_age = 80, freq = 2, uptake = 1)
  alter_val = list(
    ages = cbind(
      c(60, 60, 50, 55, 55),
      c(75, 85, 80, 80, 75)
    ), 
    uptake = c(0.4, 0.7),
    freq = c(1,3:5)
  )
}
plot_data = lapply(c(metrics,'ICER'), function(var0){
  list(
    lapply(1:nrow(ages), function(i){
      x1 = proj_sb0%>%
        filter(Smoking_ban_policy==scenario_proj[1],
               min_age == base_stra$min_age, max_age == base_stra$max_age, 
               freq == base_stra$freq, uptake == base_stra$uptake)%>%
        arrange(Strategy, Seed)%>%
        pull(!!as.symbol(var0))
      x2 = proj_sb0%>%
        filter(
          Smoking_ban_policy==scenario_proj[1],
          min_age == alter_val$ages[i,1], max_age == alter_val$ages[i,2], 
          freq == base_stra$freq, uptake == base_stra$uptake)%>%
        arrange(Strategy, Seed)%>%
        pull(!!as.symbol(var0))
      x2/x1
    }), 
    lapply(alter_val$uptake, function(i){
      x1 = proj_sb0%>%
        filter(
          Smoking_ban_policy==scenario_proj[1],
          min_age == base_stra$min_age, max_age == base_stra$max_age, 
          freq == base_stra$freq, uptake == base_stra$uptake)%>%
        arrange(Strategy, Seed)%>%
        pull(!!as.symbol(var0))
      x2 = proj_sb0%>%
        filter(
          Smoking_ban_policy==scenario_proj[1],
          min_age == base_stra$min_age, max_age == base_stra$max_age, 
          freq == base_stra$freq, uptake == i)%>%
        arrange(Strategy, Seed)%>%
        pull(!!as.symbol(var0))
      x2/x1
    }),
    lapply(alter_val$freq, function(i){
      x1 = proj_sb0%>%
        filter(
          Smoking_ban_policy==scenario_proj[1],
          min_age == base_stra$min_age, max_age == base_stra$max_age, 
          freq == base_stra$freq, uptake == base_stra$uptake)%>%
        arrange(Strategy, Seed)%>%
        pull(!!as.symbol(var0))
      x2 = proj_sb0%>%
        filter(
          Smoking_ban_policy==scenario_proj[1],
          min_age == base_stra$min_age, max_age == base_stra$max_age, 
          freq == i, uptake == base_stra$uptake)%>%
        arrange(Strategy, Seed)%>%
        pull(!!as.symbol(var0))
      x2/x1
    })
  )
})

n.vars= length(plot_data); n.cat = length(plot_data[[1]])
n.lines = lapply(1:n.cat, function(i) plot_data[[1]][[i]]%>%length)%>%unlist
x_names = c(metric_name,'ICER')
y_names = c(paste0('Age range (',base_stra$min_age,'-',base_stra$max_age,' yrs)'), 
            'Uptake (100%)', 
            paste0('Frequency (',freq_name[base_stra$freq],')'))
y_labs = list(
  paste0(alter_val$ages[,1], '-', alter_val$ages[,2]),
  paste0(alter_val$uptake*100, '%'),
  freq_name_s[alter_val$freq]
)

png(paste0(plot.dr,'ratios_baseline_',ver,'_',ver1,'.png'),width=30, height=15, units='cm', res=300, pointsize=10)
pushViewport(plotViewport(c(1,3,1.5,4)))
pushViewport(viewport(layout = grid.layout(nrow=n.cat,ncol=n.vars, heights = unit(n.lines+1, "null"))))
for(k in 1:n.vars){
  for(m in 1:n.cat){
    pushViewport(viewport(layout.pos.row = m,layout.pos.col = k))
    # xrange = range(unlist(plot_data[[k]][[m]])) + c(-1,1) * 0.05
    xrange = scale_up(range(unlist(plot_data[[k]][[m]])), abs_scale = T, delta = 0.05)
    if(k==6) xrange = c(0.95, 1.05)
    
    n1 = n.lines[m]
    yrange = c(0, n1)+0.5; ytk = 0:n1+0.5; ytk1 = 1:n1
    if(k==1){
      ylab = y_labs[[m]]%>%rev
    }else{
      ylab = rep('', n1)
    }
    xtk1 = xtk = xlab = find_xtk(xrange)
    margin=c(2,0.5,1,1)
    
    clr = c('darkslateblue', 'firebrick')
    for(i in 1:n1){
      y = plot_data[[k]][[m]][[i]]
      if(length(y)>0){
        for(j in 1:2){
          y.pos = n1 + 1 - i - (j-1.5)*0.3
          s_para = 0.1
          if(ver1 == 'median'){
            p_alpha = 0.6; p_size = 0.3
          }else if(ver1 == 'all'){
            p_alpha = 0.3; p_size = 0.2
          }else{
            p_alpha = 0.35; p_size = 0.2
          }
          if(j==1) y1 = y[1:round(length(y)/2)]
          if(j==2) y1 = y[-c(1:round(length(y)/2))]
          plot_linedots(list(y1), x_idx = rep(y.pos, length(y1)), ci = F, col = clr[j], 
                        scatter = T, point_alpha = p_alpha, scatter_para = s_para,
                        value_at_y = F, point_style = 15+j, cex = p_size)
        }
        
      }
    }
    y_text=''; x_text = 'Ratio'
    if(xrange[1]<1 & xrange[2]>1){
      v_ref =1
    }else{
      v_ref = NA
    }
    plot_xy(xmargin = (m==n.cat), ymargin = F, plot_y = T, plot_x = T,  closer_x = 0.5, 
            v_ref = v_ref, ref_col = 'grey70', auto_xaxis = T,
            x_name = ifelse(m==1, x_names[k], NA), y_name = ifelse(k==1, y_names[m], NA),
            y_name_pos = 2.7, x_name_pos = 1,
            plot_label = paste0('(', letters[(m-1)*n.vars+k],')'),plot_label_above = 0.5,
            customize_yaxis = T, customize_xaxis = T, lab_size_y = 0.8, lab_pos_x = 1)
    popViewport()
  }
}
popViewport()
plot_legend(gender, 
            col=clr, n.points = 2, topright = F, 
            point_style = 16, cex = 0.5)

popViewport()
dev.off()

#scatter plot for all strategies-------------
vars = c('QALY_raw', 'Late.cases.diagnosed','Total.deaths','Cost','OverDiagnosis', 'False.Positives','ICER'); n.vars = length(vars)
var_name = list('QALY (million)', 'Late-satge cases (thousand)','Deaths (thousand)',
                'Cost (billion SGD)', 'Overdiagnosis (thousand)','False positives (thousand)',
                'ICER  (thousand SGD/QALY)')
proj_sb = proj_clean(proj, scale_all = F, new_derive = T, agg_by_seed = T)
proj_sb1 = proj_sb%>%
  arrange(min_age, max_age)%>%
  mutate(age = paste0(min_age, '-', max_age), 
         Late.cases.diagnosed = Late.cases.diagnosed/1e3, Total.deaths = Total.deaths/1e3,
         OverDiagnosis = OverDiagnosis/1e3, False.Positives = False.Positives/1e3,
         QALY_raw = QALY_raw / 1e6, Cost = Cost/1e9, ICER = ICER/1e3)
age_ranges = unique(proj_sb1$age); n.age = length(age_ranges)
plot_data = lapply(0:1, function(g){ #2 gender
  lapply(vars, function(k){
    lapply(age_ranges, function(a){
      lapply(1:n.scenario, function(m){ #tobacco control scenarios
        proj_sb1%>%
          filter(screen_gender == g, age == a,
                 Smoking_ban_policy == scenario_proj[m])%>%
          arrange(uptake, freq)%>%
          pull(!!as.symbol(k))
      })
    })
  })
})

n1 = n.freq * n.uptake
if(ver == 'aggressive'){
  ylim_list = list(c(85.7, 85.83), c(108,145),c(50,73),c(9, 12.5), c(0, 4.3), c(0, 800),c(25,65))
  ytk_list = list(seq(85.7, 85.82, 0.05), 11:14*10, seq(50,70,10), 9:12, 0:2*2, 0:2*400, 3:6*10)
}else if(ver == 'conservative'){
  ylim_list = list(c(85.6, 85.77), c(108,140),c(100,135),c(7, 10.5), c(0, 4.3), c(0, 800),c(30,67))
  ytk_list = list(seq(85.6, 85.77, 0.05), 11:14*10, 10:13*10, 7:10, 0:2*2, 0:2*400, 3:6*10)
}
point_list = c(0:2, 7, 13, 11, 15:17)
clr = color_list$scenario

base_ref = proj%>%
  filter(screen == 0, Smoking_ban_policy == 'Baseline')%>%
  rename(QALY_raw = !!as.symbol(ifelse('QALY_raw'%in%names(proj), 'QALY_raw', 'QALY')))%>%
  mutate(QALY_raw = QALY_raw/1e6, Cost = Cost/1e9, Total.deaths = Total.deaths/1e3, 
         # OverDiagnosis = OverDiagnosis/1e3, False.Positives = False.Positives/1e3,
         Late.cases.diagnosed = Late.cases.diagnosed/1e3)%>%
  select(all_of(vars[!vars%in%c('OverDiagnosis','False.Positives','ICER')]))%>%
  summarise(across(where(is.numeric), \(x) median(x, na.rm = TRUE)))

png(paste0(plot.dr,'scatter_CEA_',ver,'_separate.png'),width=25, height=n.vars*4+2, units='cm', res=300,pointsize=10)
pushViewport(plotViewport(c(2,4.5,0.5,6.5)))
pushViewport(viewport(layout = grid.layout(nrow=n.vars,ncol=n.gender)))
for(g in 1:n.gender){ #gender
  for(k in 1:n.vars){
    pushViewport(viewport(layout.pos.row = k,layout.pos.col = g))
    xrange = c(0, n1) + 0.5; xtk = 0:n1 + 0.5; xtk1 = 1:n1; xlab = rep(freq_name_s,3)
    yrange = ylim_list[[k]] 
    margin=c(1,0,1,1)
    y_text=''; x_text = ''
    for(i in 1:n.age){
      x0 = seq(-1,1, length.out = n1) * 0.4 
      for(j in 1:n.scenario){
        plot_linedots(list(plot_data[[g]][[k]][[i]][[j]]), x_idx=1:n1+x0[i], col=clr[j], 
                      point_style = point_list[i],  ci=F, point_alpha = 0.7, cex = 0.3)
      }
    }
    if(vars[k]%in%names(base_ref)){
      h_ref = base_ref[[vars[k]]][1]
      if(h_ref > yrange[2]) h_ref = NA
    }else{
      h_ref = NA
    }
    y_text = var_name[k]; x_text = ''
    ytk1 = ytk = ylab = ytk_list[[k]]
    if(g==2) ylab = rep('', length(ytk))
    plot_xy(xmargin = (k==n.vars), ymargin = (g==1), plot_y = T, plot_x = T,  closer_y = ifelse(k==1, -0.5, 0.3), 
            h_ref = h_ref, ref_col = 'grey70',
            customize_yaxis = T, customize_xaxis = T, lab_size_x = 0.7, lab_pos_x = 0.8, lab_pos_y = 0.8,
            x_name = ifelse(k==1, gender[g], NA), y_text_size = 0.8, 
            plot_label = paste0('(', letters[(k-1)*n.gender+g],')')
    )
    if(k==n.vars){
      x1 = seq(0, n1-0.5, length.out =4)/n1
      for(i in 1:n.uptake){
        y.pos = unit(-1,'lines'); x.pos = (x1[i] + x1[i+1])/2
        delta.x = c(3.3,4.3)/n1/2
        grid.text(paste0(uptakes[i], '% uptake'), x.pos, y.pos, 
                  gp=gpar(font=2))
        grid.lines(x.pos - delta.x, y.pos, gp=gpar(lwd=1.5))
        grid.lines(x.pos + delta.x, y.pos, gp=gpar(lwd=1.5))
      }
    }
    popViewport()
  }
}
popViewport()
plot_legend(c(scenario_name, age_ranges), 
            col=c(clr, rep('black',n.age)), n.points = n.age+n.scenario, topright = F, 
            point_style = c(rep(16, n.scenario), point_list), cex = 0.7)
popViewport()
dev.off()

#screening sensitivity analysis plot
plot_data = lapply(0:1, function(g){ #2 gender
  lapply(vars, function(k){
    lapply(age_ranges, function(a){
      lapply(1:3, function(m){ #tobacco control scenarios
        proj_sb_sens[[m]]%>%
          arrange(min_age, max_age)%>%
          mutate(age = paste0(min_age, '-', max_age), 
                 Late.cases.diagnosed = Late.cases.diagnosed/1e3, Total.deaths = Total.deaths/1e3,
                 OverDiagnosis = OverDiagnosis/1e3, False.Positives = False.Positives/1e3,
                 QALY_raw = QALY_raw / 1e6, Cost = Cost/1e9, ICER = ICER/1e3)%>%
          filter(screen_gender == g, age == a)%>%
          arrange(uptake, freq)%>%
          pull(!!as.symbol(k))
      })
    })
  })
})
ylim_list = list(c(85.7, 85.78), c(120,145),c(63,73),c(10, 13), c(0, 5), c(0, 1000),c(20,60))
ytk_list = list(seq(85.7, 85.78, 0.03), 12:14*10, seq(63,73,3), 10:13, 0:2*2, 0:2*400, 1:3*20)
sens_name = c('Low-sensitivity', 'Main analysis', 'High-sensitivity')
n.sens = 3
clr = viridis(n.sens*2, option = 'D')[1:n.sens*2-1]
png(paste0(plot.dr,'scatter_CEA_sens_separate.png'),width=25, height=n.vars*4+2, units='cm', res=300,pointsize=10)
pushViewport(plotViewport(c(2,4.5,0.5,6.5)))
pushViewport(viewport(layout = grid.layout(nrow=n.vars,ncol=n.gender)))
for(g in 1:n.gender){ #gender
  for(k in 1:n.vars){
    pushViewport(viewport(layout.pos.row = k,layout.pos.col = g))
    xrange = c(0, n1) + 0.5; xtk = 0:n1 + 0.5; xtk1 = 1:n1; xlab = rep(freq_name_s,3)
    yrange = ylim_list[[k]] 
    margin=c(1,0,1,1)
    y_text=''; x_text = ''
    for(i in 1:n.age){
      x0 = seq(-1,1, length.out = n1) * 0.4 
      for(j in 1:n.sens){
        plot_linedots(list(plot_data[[g]][[k]][[i]][[j]]), x_idx=1:n1+x0[i], col=clr[j], 
                      point_style = point_list[i],  ci=F, point_alpha = 0.7, cex = 0.3)
      }
    }
    if(vars[k]%in%names(base_ref)){
      h_ref = base_ref[[vars[k]]][1]
      if(h_ref > yrange[2]) h_ref = NA
    }else{
      h_ref = NA
    }
    y_text = var_name[k]; x_text = ''
    ytk1 = ytk = ylab = ytk_list[[k]]
    if(g==2) ylab = rep('', length(ytk))
    plot_xy(xmargin = (k==n.vars), ymargin = (g==1), plot_y = T, plot_x = T,  closer_y = ifelse(k==1, -0.5, 0.3), 
            h_ref = h_ref, ref_col = 'grey70',
            customize_yaxis = T, customize_xaxis = T, lab_size_x = 0.7, lab_pos_x = 0.8, lab_pos_y = 0.8,
            x_name = ifelse(k==1, gender[g], NA), y_text_size = 0.8, 
            plot_label = paste0('(', letters[(k-1)*n.gender+g],')')
    )
    if(k==n.vars){
      x1 = seq(0, n1-0.5, length.out =4)/n1
      for(i in 1:n.uptake){
        y.pos = unit(-1,'lines'); x.pos = (x1[i] + x1[i+1])/2
        delta.x = c(3.3,4.3)/n1/2
        grid.text(paste0(uptakes[i], '% uptake'), x.pos, y.pos, 
                  gp=gpar(font=2))
        grid.lines(x.pos - delta.x, y.pos, gp=gpar(lwd=1.5))
        grid.lines(x.pos + delta.x, y.pos, gp=gpar(lwd=1.5))
      }
    }
    popViewport()
  }
}
popViewport()
plot_legend(c(sens_name, age_ranges), 
            col=c(clr, rep('black',n.age)), n.points = n.age+3, topright = F, 
            point_style = c(rep(16, 3), point_list), cex = 0.7)
popViewport()
dev.off()


#SI plot
#Figure S1: observed prob of starting smoking /quitting by age---------
cum_prob = lapply(0:1, function(i){
  lapply(tolower(gender), function(j){
    dat = SI_dat[[1]]%>%
      filter(type == paste0(i, '-', i+1), gender == j)%>%
      select(age, prob, lower, upper)%>%
      # mutate(across(all_of(c('prob', 'lower', 'upper')), function(x) x*100))%>%
      as.matrix
    dat  = rbind(c(0, rep(1, 3)), dat)
    cumdf_to_plot_mat(dat)
  })
})
plot_data = lapply(1:2, function(i){
  lapply(1:n.gender, function(j){
    list(cum_prob[[i]][[j]]$y * 100)
  })
})
x_idxs = lapply(1:2, function(i){
  lapply(1:n.gender, function(j){
    cum_prob[[i]][[j]]$x
  })
})
xrange=c(0, 85); xlab = xtk = 0:4 * 20
yrange = c(0, 110)
clr = with(color_list, c(male_fit, female_fit))
png(paste0(plot.dr,'FigureS1.png'),width=16, height=8, units='cm', res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,0,0)))
pushViewport(viewport(layout = grid.layout(nrow=1,ncol=2)))
for(k in 1:2){
  pushViewport(viewport(layout.pos.row = 1,layout.pos.col = k))
  margin=c(1.5,2,1,1)
  y_text='Probability (%)'; x_text='Age (years)'
  for(i in 1:n.gender){
    plot_lines(plot_data[[k]][[i]], x_idx = x_idxs[[k]][[i]], col=clr[i])
  }
  
  plot_xy(xmargin = T, ymargin = (k==1), closer_y = 0.1,
          plot_label = paste0('(', letters[k],') '), plot_label_above = F)
  if(k==2) plot_legend(gender, col = clr, topright = T, topright_pos = 4)
  popViewport()
}
popViewport()
popViewport()
dev.off()


#Figure S2 somking intensity---------------
# emp_cdf = lapply(1:2, function(i){
#   SI_dat[[2]]%>%
#     filter(gender_code==i)%>%
#     select(cigarettes_per_day, cumulative_prob)%>%
#     as.matrix()%>%
#     cumdf_to_plot_mat
# })
# plot_data = lapply(1:n.gender, function(i){
#   matrix(emp_cdf[[i]]$y *100, nrow = 1)
# })
# x_idxs = lapply(1:n.gender, function(i){
#   emp_cdf[[i]]$x/20
# })
# xrange = c(0, 3.2); xtk = xlab = 0:3
# yrange = c(0, 110)
# clr = with(color_list, c(male_fit, female_fit))
# png(paste0(plot.dr,'FigureS2.png'),width=10, height=8, units='cm', res=300,pointsize=10)
#   margin=c(3.5,4,1,5)
#   y_text='Cumulative probability (%)'; x_text='Somking intensity (packs/day)'
#   for(i in 1:n.gender){
#     plot_lines(plot_data[[i]], x_idx = x_idxs[[i]], col=clr[i], ci = F)
#   }
#   plot_xy(xmargin = T, ymargin = T, closer_y = 0.1)
#   plot_legend(rev(gender), col = rev(clr), topright = F, n.lines = n.gender)
# dev.off()
obs_freq = list(
  c(288, 292, 41, 35, 8), 
  c(30, 12, 1, 0, 0)
)
plot_data = lapply(1:2, function(i){
  obs_freq[[i]]/sum(obs_freq[[i]]) * 100
})
xrange = c(-80, 80); xtk = seq(-60, 60, 20); xlab = abs(xtk)
clr = with(color_list, c(male_fit, female_fit))
png(paste0(plot.dr,'FigureS2_turnado.png'),width=12, height=8, units='cm', res=300,pointsize=10)
margin=c(3.5,4,1,1)
plot_turnado(plot_data, y_name = 'Somking intensity (packs/day)', 
             cat_name = gender, xrange = xrange, yname_pos = 2.7, display_val = T, val_round = 1)
x_text = 'Probability (%)'
ylab = 1:5/2; ytk = 0:5; ytk1 = 5:1 -0.5; yrange = c(0, 5)
plot_xy(customize_yaxis = T, xmargin = T, ymargin = F)
dev.off()


#Figure S3 cost fitting----------
plot_data = lapply(paste0(c('non-', ''), 'smokers'), function(k){
  # lapply(c('G1', 'G3/G4'), function(i){
    lapply(c('I','II','III','IV'), function(j){
      dat1 = cost_fit%>%
        filter(smoking_grp==k, stage == paste0('Stage ', j))%>%
        select(predicted_cost, lower_CI, upper_CI)%>%
        as.matrix()
      dat1/1e3
    })
  # })
})
obs_dat = lapply(paste0(c('non-', ''), 'smokers'), function(k){
  # lapply(c('G1', 'G3/G4'), function(i){
    lapply(c('I','II','III','IV'), function(j){
      dat1 = cost_obs%>%
        filter(smoking_grp==k, stage == paste0('Stage ', j))%>%
        # group_by(YEAR)%>%
        # summarise(obs_cost = mean(obs))%>%
        pull(obs_cost)
      dat1/1e3
    })
  #})
})
xrange = c(1, 10.5); xtk = xlab = seq(2, 10, 2)
yrange = c(0, 100)
y_text='Cost (thousand SGD)'; x_text='Time from diagnosis (yr)'
y_name = c('Never-smoker', 'Ever-smoker'); x_name = c('Grade 1', 'Grade 3-4')
clr = color_list$stage_fit; clr_1= color_list$stage_obs
n.col = 2; n.row =  1
png(paste0(plot.dr,'cost_stage.png'),width=n.col*6+6, height=8*n.row, units='cm', res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,8.5)))
pushViewport(viewport(layout = grid.layout(nrow=n.row,ncol=n.col)))
# for(i in 1:n.row){
  for(j in 1:n.col){
    pushViewport(viewport(layout.pos.row = 1,layout.pos.col = j))
    margin=c(2,2,0.5,1)
    plot_lines(plot_data[[j]], col=clr)
    for(m in 1:n.stage){
      plot_linedots(list(obs_dat[[j]][[m]]), col=clr_1[m], point_style = 14+m, 
                    cex=0.5, ci=F)
    }
    plot_xy(xmargin = T, ymargin = (j==1),
            y_name_pos = 4,
            plot_label = paste0('(', letters[j],') ', y_name[j]))
    popViewport()
  }
# }
popViewport()
plot_legend(c(paste0(rev(stage),' (Modelled)'), paste0(rev(stage),' (Observed)')),
            col=c(rev(clr), rev(clr_1)), n.points = 4, point_style = 15:18, cex = 0.6,
            topright = F)
popViewport()
dev.off()


#SI plot: individual metric (without scaling)----------
# var = 'ICER'; xrange = c(3e4, 7.1e4); xtk = xtk1 =3:7*1e4; xlab = xtk/1e3; x_text='ICER (thousand SGD/QALY)'
# s = 1
# for(s in 1:n.scenario)
# {
#   scenario0 = scenario_proj[s]
#   proj_dat=lapply(0:1, function(i){ #by gender
#     dat1 = lapply(1:n.freq, function(k){ #by frequency
#       proj_sb%>%
#         filter(screen_gender == i, freq == k, Smoking_ban_policy == scenario0)%>%
#         arrange(uptake, min_age, max_age)
#     })
#     outcome = lapply(1:n.freq, function(k){
#       dat1[[k]]%>%pull(var)
#     })
#     strategy = dat1[[1]]%>%
#       mutate(cat = paste0(min_age, '-', max_age))%>%
#       pull(cat)
#     list(outcome = outcome, strategy = strategy)
#   })
#   clr = color_list$freq_g
#   
#   png(paste0(plot.dr,ver,'_',scenario_name[s],'_',var,'.png'),width=16, height=20, units='cm', res=300,pointsize=10)
#   pushViewport(plotViewport(c(3,3,1,10)))
#   # pushViewport(viewport(layout = grid.layout(nrow=1,ncol=2)))
#   for(k in 1:2){
#     # pushViewport(viewport(layout.pos.row = 1,layout.pos.col = k))
#     n.rows = length(proj_dat[[k]]$strategy)
#     yrange = c(0, n.rows) + 0.5; ytk = 0:n.rows + 0.5; ytk1 = 1:n.rows; ylab = proj_dat[[k]]$strategy
#     #xtk=0:n.cats; xlab = rep('', n.cats+1)
#     margin=c(1,1,0.5,0.5)
#     y_text=''; 
#     plot_linedots(proj_dat[[k]]$outcome, x_idx = 1:n.rows, ci = F, col = clr[,k], 
#                   value_at_y = F, point_style = 14+k, cex = 0.7)
#     if(k==1) plot_xy(xmargin = T, ymargin = F, plot_y = T, plot_x = T, 
#                      group = paste0(c(40, 70, 100)%>%rev, '% Uptake'), group_pos = 0:3 * (n.rows/3), group_at_x = F, 
#                      customize_yaxis = T, customize_xaxis = T, lab_size_y = 0.8, lab_pos_x = 0.8, closer_x = 0.5)
#     # popViewport()
#   }
#   # popViewport()
#   plot_legend(lapply(1:2, function(i) paste0(freq_name, ' (', gender[i], ')'))%>%unlist, 
#               col=clr%>%as.vector(), n.points = n.freq*2, topright = F, 
#               point_style = rep(15:16, each= n.freq), cex = 0.7)
#   popViewport()
#   dev.off()
# }

#screening sensitivity level plots--------------
sens_scenario = c('low', 'high')
sens_name = c('Low-sensitivity', 'Main analysis', 'High-sensitivity')
n.sens = 3
proj_sb_sens = lapply(1:3, function(k){
  proj_sb = proj_clean(proj_sens[[k]]%>%filter(Smoking_ban_policy == 'Baseline'), scale_all = F, new_derive = T, agg_by_seed = T)
})
# for(s in 1:2)
# {
#   proj_dat=lapply(0:1, function(i){ #by gender
#     dat1 = lapply(1:n.freq, function(k){ #by frequency
#       proj_sb_sens[[ifelse(s==1, 1, 3)]]%>%
#         filter(screen_gender == i, freq == k, Smoking_ban_policy == 'Baseline')%>%
#         arrange(uptake, min_age, max_age)
#     })
#     outcome = lapply(1:n.freq, function(k){
#       dat1[[k]]%>%
#         select(all_of(radar_metrics))%>%
#         as.matrix
#     })
#     strategy = dat1[[1]]%>%
#       mutate(cat = paste0(min_age, '-', max_age))%>%
#       pull(cat)
#     list(outcome = outcome, strategy = strategy)
#   })
#   clr = color_list$metrics
#   png(paste0(plot.dr,'screen_heatmap_',sens_scenario[s],'.png'),width=16, height=20, units='cm', res=300,pointsize=10)
#   pushViewport(plotViewport(c(0,3,1,11)))
#   pushViewport(viewport(layout = grid.layout(nrow=1,ncol=2)))
#   for(k in 1:2){
#     pushViewport(viewport(layout.pos.row = 1,layout.pos.col = k))
#     n.cats = n.metrics
#     n.rows = nrow(proj_dat[[k]]$outcome[[1]])
#     xrange = c(0, n.cats); yrange = c(0, n.rows)
#     #xtk=0:n.cats; xlab = rep('', n.cats+1)
#     margin=c(1,1,0.5,0.5)
#     y_text=''; x_text=''
#     if(k==1) {
#       add_lab = paste0(c(40, 70, 100)%>%rev, '% Uptake')
#     }else{
#       add_lab = NA
#     }
#     plot_rect(proj_dat[[k]]$outcome, row_name = proj_dat[[k]]$strategy, col_sub =n.freq, 
#               row_name2 = add_lab, line_wd = 1.5, 
#               ytk2 = 0:3 * (n.rows/3), col_name = paste0('(', letters[k], ') ', gender[k]),
#               col = clr, shade_alpha = 0.8, label = (k==1))
#     #plot_xy(xmargin = T, ymargin = F, plot_y = F, plot_x = F, x_name = gender[k] )
#     popViewport()
#   }
#   popViewport()
#   plot_legend(metric_name, col=clr, n.onlyshade = n.metrics, topright = F, shade_alpha = 0.8)
#   popViewport()
#   dev.off()
# }

proj_sb0_sens = lapply(1:n.sens, function(k){
  proj_sb = proj_clean(proj_sens[[k]]%>%filter(Smoking_ban_policy == 'Baseline'), scale_all = F, new_derive = T, agg_by_seed = F, scale = F)
})
radar_dat = lapply(1:n.sens, function(m){
  #proj_sb = proj_clean(proj, scenario0 = m)
  lapply(0:1, function(i){
    dat1 = proj_sb_sens[[m]] %>% filter(screen_gender == i)
    dat2 = proj_sb0_sens[[m]] %>% filter(screen_gender == i)
    eligible = proj_sb0_sens[[m]]%>%
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
    n.eligible = length(eligible)
    topsis_idx = mclapply(1:n.sim, function(s){
      dat2 = proj_clean(proj_sens[[m]]%>%filter(Seed == s, Smoking_ban_policy == 'Baseline'), 
                        scale_all = F, new_derive = T, agg_by_seed = F, scale = F)%>%
        filter(screen_gender == i,  Strategy %in% eligible)%>%
        proj_clean(scale = T, scale_approach = 'lognormal')
      # is_front = pareto_frontier(dat2 %>% filter(OD_rate <= 10), cols = radar_metrics, maximize = rep(T, n.metrics))
      # is_front = pareto_frontier(dat2, cols = radar_metrics, maximize = rep(T, n.metrics))
      scores = compute_topsis_scores(
        df            = dat2, 
        decision_cols = radar_metrics,
        beneficial    = rep(TRUE, n.metrics)
      )
      scores
    }, mc.cores = detectCores())%>%do.call('rbind',.)%>%as.matrix
    topsis = eligible[which.max(colMeans(topsis_idx))]
    idx = c(life_saving, most_cea, od_control, topsis)
    idx_strategy = lapply(idx, function(i){
      which(dat1$Strategy == i)
    })%>%unlist
    metric_val = dat1[idx_strategy, ] %>% select (all_of(radar_metrics)) %>% as.matrix()
    metric_val0 = dat1[idx_strategy, ] %>% select (all_of(metrics)) %>% as.matrix()
    strategy = dat1[idx_strategy, ] %>% select (Strategy, min_age, max_age, uptake, pack_yrs, quitting_t, freq)
    list(metrics = metric_val, metrics_raw = metric_val0, strategy = strategy)
  })
})

clr = color_list$strategy.g
png(paste0(plot.dr,'select_all_sens.png'),width=30, height=18, units='cm', res=300,pointsize=10)
pushViewport(plotViewport(c(0,2,2,15)))
pushViewport(viewport(layout = grid.layout(nrow=4,ncol=1)))
for(k in 1:2){
  grid.text(gender[k], unit(-0.5, 'lines'), 1.25 - k/2, gp=gpar(font=2, cex = 1.5), rot = 90)
  #radar plot
  margin=c(1, 2, 0.5, 2.5)
  pushViewport(viewport(layout.pos.row = k*2 - 1,layout.pos.col = 1))
  pushViewport(viewport(layout = grid.layout(nrow=1,ncol=n.screen_vars)))
  for(m in 1:n.sens){
    pushViewport(viewport(layout.pos.row = 1,layout.pos.col = m))
    plot_radar(radar_dat[[m]][[k]]$metrics[,radar_idx], col=clr[,k], size = 0.85, 
               shade = c(rep(F, n.strategy-1), T), shade_alpha = 0.1,
               line_alpha = c(rep(0.6, n.strategy-1), 0.8),
               line_type = c(2:n.strategy, 1), 
               label = metric_name_short[radar_idx], plot_name_bold = F,
               plot_name = paste0('(',letters[(k-1) * (n.sens + n.screen_vars)+m], ') ', sens_name[m]))
    popViewport()
  }
  popViewport()
  popViewport()
  pushViewport(viewport(layout.pos.row = k*2,layout.pos.col = 1))
  pushViewport(viewport(layout = grid.layout(nrow=1,ncol=n.screen_vars)))
  for(s in 1:n.screen_vars){
    ylab = range_list[[screen_vars[s]]]; ytk = ytk1 = seq_along(ylab)
    xtk = 0:n.strategy; xtk1 = 1: n.strategy - 0.5; xlab = rep('',n.strategy); xlab = strategy_name1
    # xtk = xtk1 = 1:n.strategy - 0.5; xlab= rep('',n.screen_vars)
    yrange = c(0, length(ylab) ); xrange = c(0, n.strategy)
    margin=c(3, 2, 2, 1)
    pushViewport(viewport(layout.pos.row = 1,layout.pos.col = s))
    plot_data = lapply(1:n.sens, function(m){
      x = radar_dat[[m]][[k]]$strategy[[screen_vars[s]]]
      x1 = lapply(x, function(i){
        if(screen_vars[s] == 'uptake'){
          which((ylab/100) == i)
        }else if(screen_vars[s] == 'freq'){
          # length(ylab) + 1 - i
          i
        }else{
          which(ylab == i)
        }
      })%>%unlist
    })
    plot_bar(plot_data, col = clr[,k], width_between = 0.3, same_col = T)
    y_text = ''; x_text = ''
    plot_xy(xmargin = F, ymargin = T, 
            plot_label = paste0('(',letters[(k-1) * (n.sens + n.screen_vars)+s + n.sens],') ', screen_varname[s]),
            closer_y = 0.6, plot_label_above = T, lab_size_x = 0.5, lab_pos_x = 0.8,
            lab_size_y = ifelse(screen_vars[s]=='freq',0.6, 1),
            customize_yaxis = T, customize_xaxis = T,lab_pos_y = 0.5)
    popViewport()
  }
  popViewport()
  popViewport()
}
popViewport() 
plot_legend(name_all, col=clr%>%as.vector(), topright = F, right_pos = 2, line_length = 1.5,
            n.lines = n.metrics * 2, line_type = rep(c(2:n.strategy, 1),2))
popViewport()
dev.off()

#scatter plot
# vars = c('QALY_raw', 'Cost', 'ICER'); n.vars = length(vars)
# proj_dat = lapply(vars, function(var0){
#   lapply(1:n.freq, function(i){
#     lapply(0:1, function(j){
#       lapply(1:n.sens, function(k){
#         proj_sb_sens[[k]]%>%
#           filter(freq == i, screen_gender == j, Smoking_ban_policy == 'Baseline')%>%
#           mutate(QALY = QALY /1e3, ICER = ICER/1e3, Cost_Savings = Cost_Savings/1e6,
#                  QALY_raw = QALY_raw/1e6, Cost = Cost/1e9)%>%
#           pull(var0)
#       })
#     })
#   })
# })
# xlim_list = list(c(85.7, 85.77), c(10.2, 12.6), c(22,52))
# xtk_list = list(seq(85.7, 85.76, 0.02), seq(10.5, 12.5, 0.5), seq(25, 50, 5))
# var_name = list('QALY (million)', 'Cost (billion SGD)', 'ICER  (thousand SGD/QALY)')
# clr = color_list$freq_g
# point_list = list(c(15:18,4), c(15:18,4))
# png(paste0(plot.dr,'scatter_CEA_sens.png'),width=16, height=n.vars*4.5+2, units='cm', res=300,pointsize=10)
# pushViewport(plotViewport(c(0,5,0.5,10)))
# pushViewport(viewport(layout = grid.layout(nrow=n.vars,ncol=1)))
# for(k in 1:n.vars){
#   pushViewport(viewport(layout.pos.row = k,layout.pos.col = 1))
#   yrange = c(0, n.sens) + 0.5; ytk = 0:n.sens + 0.5; ytk1 = n.sens:1; ylab = sens_name
#   xrange = xlim_list[[k]]; xtk1 = xtk = xlab = xtk_list[[k]]
#   margin=c(2,0,1,1)
#   y_text=''; x_text = ''
#   {
#     for(j in 1:2){ #gender
#       for(m in 1:n.freq){
#         for(n in 1:n.sens){
#           y = proj_dat[[k]][[m]][[j]][[n]]
#           plot_linedots(list(y), x_idx = rep(n.sens + 1 - n, length(y)), ci = F, col = clr[m,j], 
#                         scatter = T,
#                         value_at_y = F, point_style = point_list[[j]][m], cex = 0.4)
#         }
#       }
#     }
#   }
#   if(vars[k]%in%names(base_ref)){
#     v_ref = base_ref[[vars[k]]][1]
#   }else{
#     v_ref = NA
#   }
#   plot_xy(xmargin = T, ymargin = T, plot_y = T, plot_x = T,  closer_x = 0.5, 
#           v_ref = v_ref, ref_col = 'grey70',
#           customize_yaxis = T, customize_xaxis = T, lab_size_y = 0.8, lab_pos_x = 1,
#           plot_label = paste0('(', letters[k],') ', var_name[k]), plot_label_above = 1)
#   popViewport()
# }
# popViewport()
# plot_legend(lapply(1:2, function(i) paste0(freq_name, ' (', gender[i], ')'))%>%unlist, 
#             col=clr%>%as.vector(), n.points = n.freq*2, topright = F, 
#             point_style = unlist(point_list), cex = 0.5)
# popViewport()
# dev.off()

#sensitivity analysis: packyear--------------
clr = cbind(c('darkslateblue', 'turquoise3'), c('tomato2','orange2'))
metrics1 = c(metrics,'ICER'); n.metrics1 = length(metrics1)
metric1_name = c('QALYs gained (thousand)', 'LSA (thousand)', 'Deaths averted (thousand)', 
                 'AddCost (billion SGD)', 'Overdiagnosis (%)',  'False positive (%)', 'ICER (thousand SGD/QALY)')
quit_time = (proj$male_ever_smoker_quitting_period%>%unique%>%sort)[-1]; n.quit = length(quit_time)
pack_yr = (proj$male_ever_smoker_pack_yrs%>%unique%>%sort)[-1]; n.pack = length(pack_yr)

proj_sb = proj_clean(proj, scale_all = F, new_derive = T, agg_by_seed = T, filter_packyr = F)
plot_data=lapply(metrics1, function(k){
  lapply(1:n.scenario, function(m){
    lapply(0:1, function(i){
      lapply(quit_time, function(j){
        out = proj_sb %>%
          filter(screen_gender==i, Smoking_ban_policy==scenario_proj[m], quitting_t == j) %>%
          arrange(pack_yrs)%>%
          select(all_of(paste0(k, c('', '_l','_u'))))%>%
          as.matrix
        if(stringr::str_detect(k,'rate')){
          out
        }else if(k == 'Cost_Savings'){
          out/1e9
        }else{
          out/1e3
        }
      })
    })
  })
})
if(ver0=='_l'){
  #most life-saving
  yrange_list = list(
    c(0, 80), #1e3
    c(0,18), #1e3
    c(0, 9), #1e3
    c(0, 4), #1e9
    c(5, 11),
    c(25, 27), 
    c(0, 140) #1e3
  )
}else{
  yrange_list = list(
    c(0, 48), #1e3
    c(0,11), #1e3
    c(0, 6), #1e3
    c(0, 1.3), #1e9
    c(4, 9.5),
    c(24.5, 26.7), 
    c(15, 70) #1e3
  )
}


png(paste0(plot.dr,'proj_packyr', ver0,'.png'),width=25, height=25, units='cm', res=300,pointsize=10)
pushViewport(plotViewport(c(1,2,1,6)))
pushViewport(viewport(layout = grid.layout(nrow=n.metrics1,ncol=n.scenario)))
for(k in 1:n.metrics1){
  for(m in 1:n.scenario){
    pushViewport(viewport(layout.pos.row = k,layout.pos.col = m))
    xrange=c(0, n.pack)+0.5; yrange=yrange_list[[k]]
    xlab=pack_yr; xtk= 0:n.pack + 0.5; xtk1 = 1:n.pack
    margin=c(1.5,2,0.5,1)
    y_text= metric1_name[k]; x_text='Smoking pack-year'
    for(i in 1:2){ #gender
      for(j in 1:n.quit){
        plot_linedots(list(plot_data[[k]][[m]][[i]][[j]]), x_idx=1:n.pack+(i+ (j-1) * 2 - (n.quit * 2 + 1) /2) *0.15 , col=clr[j,i], 
                      linkpoints = T, line_wd = 1.5, point_alpha = 0.6, line_alpha = 0.5, point_style = 14 + j + (i-1) * n.quit)
      }
    }
    
    plot_xy(xmargin = (k == n.metrics1), ymargin = F,
            customize_xaxis = T, closer_x= 0.7, lab_size_x = 0.7, lab_pos_x = 0.8, 
            x_name = ifelse(k==1, scenario_name[m], ''), 
            y_name = ifelse(m==1, metric1_name[k], ''), y_name_size = 0.8, y_name_pos = 3.8)
    
    if(k==ceiling((n.metrics1+1)/2) & m == n.scenario) 
      plot_legend(lapply(gender, function(g){paste0(quit_time,' (',g,')')})%>%unlist,
                  col=as.vector(clr), n.pointlines = 2*n.quit, point_style = 14 + 1:(2*n.quit),
                  topright = F)
    popViewport()
  }
}
popViewport()
popViewport()
dev.off()
