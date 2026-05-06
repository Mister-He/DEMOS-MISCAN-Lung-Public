#utility functions--------------
#mean and 95% CI
estim=function(dat, vector=F, median_val = F){
  if(vector) dat=matrix(dat, ncol=1)
  mclapply(1:ncol(dat), function(k){
    mu = ifelse(median_val, median(dat[,k]), mean(dat[,k]))
    c(mu, quantile(dat[,k], c(0.025,0.975)))
  }, mc.cores = detectCores())%>%do.call('rbind',.)%>%as.matrix
}

#pareto frontier
pareto_frontier <- function(df, cols = NULL, maximize = NULL, na.rm = TRUE) {
  if (is.null(cols)) {
    cols <- names(df)[vapply(df, is.numeric, logical(1))]
    if (length(cols) == 0) stop("No numeric columns found. Please specify `cols`.")
  } else {
    if (!all(cols %in% names(df))) stop("Some `cols` not present in df.")
  }
  nobj <- length(cols)
  if (is.null(maximize)) {
    maximize <- rep(TRUE, nobj)
  } else {
    if (length(maximize) != nobj) stop("`maximize` length must match `cols`.")
    if (!all(maximize %in% c(TRUE, FALSE))) stop("`maximize` must be TRUE/FALSE.")
  }
  m <- nrow(df)
  obj_mat <- matrix(NA_real_, nrow = m, ncol = nobj)
  colnames(obj_mat) <- cols
  for (k in seq_len(nobj)) {
    colk <- cols[k]
    vec <- df[[colk]]
    if (!is.numeric(vec)) stop(sprintf("Column `%s` is not numeric.", colk))
    if (na.rm) {
      if (maximize[k]) {
        vec[is.na(vec)] <- -Inf
      } else {
        vec[is.na(vec)] <- +Inf
      }
    } else {
      if (maximize[k]) {
        vec[is.na(vec)] <- -Inf
      } else {
        vec[is.na(vec)] <- +Inf
      }
    }
    if (!maximize[k]) vec <- -vec
    obj_mat[, k] <- vec
  }
  is_pareto_matrix <- function(mat) {
    n <- nrow(mat)
    dominated <- rep(FALSE, n)
    for (i in seq_len(n)) {
      if (!dominated[i]) {
        for (j in seq_len(n)) {
          if (i != j && !dominated[j]) {
            if (all(mat[j, ] >= mat[i, ]) && any(mat[j, ] > mat[i, ])) {
              dominated[i] <- TRUE
              break
            }
          }
        }
      }
    }
    return(!dominated)
  }
  is_front <- which(is_pareto_matrix(obj_mat))
  return(is_front)
}


pareto_frontier_multi_seed <- function(df, cols = NULL, maximize = NULL, idx_var = 'Strategy') {
  df$idx = df[[idx_var]]
  if (is.null(cols)) {
    cols <- names(df)[vapply(df, is.numeric, logical(1))]
    if (length(cols) == 0) stop("No numeric columns found. Please specify `cols`.")
  } else {
    if (!all(cols %in% names(df))) stop("Some `cols` not present in df.")
  }
  nobj <- length(cols)
  if (is.null(maximize)) {
    maximize <- rep(TRUE, nobj)
  } else {
    if (length(maximize) != nobj) stop("`maximize` length must match `cols`.")
    if (!all(maximize %in% c(TRUE, FALSE))) stop("`maximize` must be TRUE/FALSE.")
  }
  idx_list = unique(df$idx) 
  m <- length(idx_list)
  dominated <- rep(FALSE, m)
  for(i in 1:m){
    if (!dominated[i]) {
      for (j in 1:m) {
        if (i != j && !dominated[j]) {
          x1 = df%>%filter(idx == idx_list[i])%>%
            select(all_of(cols))%>%as.matrix()
          x2 = df%>%filter(idx == idx_list[j])%>%
            select(all_of(cols))%>%as.matrix()
          delta = x2 - x1
          if(mean(maximize)<1){
            idx0 = which(maximize==F)
            delta[,idx0] = -delta[,idx0]
          }
          res1 = lapply(1:nobj, function(k){
            t.test(delta[,k], alternative = 'greater')$p.value<0.05
          })%>%unlist
          res2 = lapply(1:nobj, function(k){
            t.test(delta[,k], alternative = 'less')$p.value<0.05
          })%>%unlist
          if(sum(res1)>0 && sum(res2)==0){
            dominated[i] <- TRUE
            break
          }
        }
      }
    }
  }
  is_front = df$idx[which(!dominated)]
  return(is_front)
}


#TOPSIS score calculation
#    • df:             data.frame of candidates (each row is a “strategy”)
#    • decision_cols:  character vector of columns to include in TOPSIS
#    • weights:        numeric vector of length = length(decision_cols)
#    • beneficial:     logical vector (TRUE = “higher is better,” FALSE = “lower is better”)
#    • Returns:        numeric vector of TOPSIS closeness scores (one per row of df)
compute_topsis_scores <- function(df, decision_cols, weights = NULL, beneficial = NULL) {
  # 1.1) Basic checks
  if (!all(decision_cols %in% names(df))) {
    stop("Some `decision_cols` are not present in df.")
  }
  n_obj <- length(decision_cols)
  mat <- as.matrix(df[, decision_cols, drop = FALSE])
  if (is.null(weights)) {
    weights <- rep(1, n_obj)
  }
  if (length(weights) != n_obj) {
    stop("`weights` must have same length as `decision_cols`.")
  }
  if (is.null(beneficial)) {
    # default: treat every column as “beneficial” (higher → better)
    beneficial <- rep(TRUE, n_obj)
  } else {
    if (length(beneficial) != n_obj) {
      stop("`beneficial` must have length = length(decision_cols).")
    }
  }
  
  # 2) Normalize each column by its Euclidean norm
  #    r_ij = x_ij / sqrt(sum_j x_ij^2)
  norm_denoms <- apply(mat^2, 2, sum)
  norm_denoms <- sqrt(norm_denoms)
  r <- sweep(mat, 2, norm_denoms, FUN = "/")
  
  # 3) Multiply by weights to get weighted‐normalized matrix v_ij = w_j * r_ij
  if (any(weights < 0)) {
    stop("All weights must be nonnegative.")
  }
  w_mat <- matrix(weights, nrow = nrow(r), ncol = n_obj, byrow = TRUE)
  v <- r * w_mat
  
  # 4) Determine the ideal best (A^+) and ideal worst (A^-)
  #    For each objective j:
  #      if beneficial[j] == TRUE,  A^+_j = max(v_ij), A^-_j = min(v_ij)
  #      if beneficial[j] == FALSE, A^+_j = min(v_ij), A^-_j = max(v_ij)
  ideal_best <- numeric(n_obj)
  ideal_worst <- numeric(n_obj)
  for (j in seq_len(n_obj)) {
    if (beneficial[j]) {
      ideal_best[j] <- max(v[, j], na.rm = TRUE)
      ideal_worst[j] <- min(v[, j], na.rm = TRUE)
    } else {
      # “non‐beneficial” means lower is better
      ideal_best[j] <- min(v[, j], na.rm = TRUE)
      ideal_worst[j] <- max(v[, j], na.rm = TRUE)
    }
  }
  
  # 5) Compute the separation measures:
  #    d_i^+ = sqrt( sum_j (v_ij – ideal_best_j)^2 )
  #    d_i^- = sqrt( sum_j (v_ij – ideal_worst_j)^2 )
  d_pos <- apply(
    (v - matrix(ideal_best, nrow = nrow(v), ncol = n_obj, byrow = TRUE))^2,
    1, sum
  )
  d_pos <- sqrt(d_pos)
  
  d_neg <- apply(
    (v - matrix(ideal_worst, nrow = nrow(v), ncol = n_obj, byrow = TRUE))^2,
    1, sum
  )
  d_neg <- sqrt(d_neg)
  
  # 6) Compute the relative closeness to the ideal solution:
  #    C_i = d_i^- / ( d_i^+ + d_i^- )
  #    Larger C_i means closer to the ideal, so “better.”
  closeness <- d_neg / pmax(0.001, d_pos + d_neg)
  return(closeness)
}

#formating projection data (cleaning and scaling)
#scale_all: T if all scenarios are scaled together
#same_ref: whether the reference scenario is the same scross different scenarios
proj_clean = function(proj, scale_all = F, new_derive = F, scale = T, same_ref = F, 
                      agg_by_seed = F, scale_approach = 'normal', ever_smoker = T, filter_packyr = T,
                      metric_vars = c('Cost', 'Cost_Savings', 'QALY', 'QALY_raw', 'OverDiagnosis', 'OD_rate','Late.cases.diagnosed', 'LSA', 'False.Positives','FP_rate', 'Total.deaths','Deaths_Averted', 'ICER')){
  # baseline_cost = proj %>% 
  #   filter(male_ever_smoker_min_age == 0, female_ever_smoker_min_age == 0, 
  #          male_never_smoker_min_age == 0, female_never_smoker_min_age == 0, 
  #          Smoking_ban_policy == cost_baseline)%>%
  #   pull(Cost)
  if(is.null(proj$Cost_Savings) && !is.null(proj$Additional_cost)){
    proj = proj%>%rename(Cost_Savings=Additional_cost)
  }
  # if(!is.null(proj$QALY_Gains) && !is.null(proj$QALY)){
  if('QALY_Gain'%in%names(proj) && 'QALY'%in%names(proj)){ 
    proj = proj%>%rename(QALY_raw=QALY)%>%rename(QALY = QALY_Gain)
  }
  if(mean(c('ICER', metrics)%in%names(proj))<1) new_derive = T
  if(new_derive){
    if(same_ref == F){
      proj = proj %>% group_by(Smoking_ban_policy, Seed)
    }else{
      proj = proj %>% group_by(Seed)
    }
    proj = proj %>%
      # mutate(QALY_raw = QALY)%>%
      mutate(
        Cost_Savings = Cost - Cost[screen==0], 
        OverDiagnosis = Total.cases.diagnosed - Total.cases.diagnosed[screen==0],
        QALY = QALY - QALY[screen==0]
      )%>%
      mutate(
        ICER = Cost_Savings/QALY,
        OD_rate = OverDiagnosis/pmax(1, Total.screened.detected) * 100
      )%>% 
      ungroup
  }
  
  if(ever_smoker){
    proj_sb = proj%>%
      mutate(# Cost_Savings = Cost - baseline_cost,
              screen_gender = as.numeric(female_ever_smoker_min_age > 0))%>%
      filter( # male_ever_smoker_uptake == 0.7,
             # (male_ever_smoker_frequency == 1 | female_ever_smoker_frequency == 1),
             # Smoking_ban_policy %in% scenario0,
             # (male_ever_smoker_pack_yrs == 20 | female_ever_smoker_pack_yrs == 20),
             # (male_ever_smoker_quitting_period == 15 | female_ever_smoker_quitting_period == 15),
             (male_ever_smoker_min_age > 0 | female_ever_smoker_min_age > 0))%>%
      mutate(min_age = pmax (male_ever_smoker_min_age, female_ever_smoker_min_age), 
             max_age = pmax (male_ever_smoker_max_age, female_ever_smoker_max_age), 
             pack_yrs = pmax (male_ever_smoker_pack_yrs, female_ever_smoker_pack_yrs), 
             quitting_t = pmax(male_ever_smoker_quitting_period, female_ever_smoker_quitting_period), 
             uptake = pmax(male_ever_smoker_uptake, female_ever_smoker_uptake),
             freq = pmax(male_ever_smoker_frequency, female_ever_smoker_frequency))
    if(filter_packyr){
      proj_sb = proj_sb%>%
        filter(
          (male_ever_smoker_pack_yrs == 20 | female_ever_smoker_pack_yrs == 20),
          (male_ever_smoker_quitting_period == 15 | female_ever_smoker_quitting_period == 15)
        )
    }
  }else{
    proj_sb = proj%>%
      mutate(# Cost_Savings = Cost - baseline_cost,
        screen_gender = as.numeric(female_never_smoker_min_age > 0))%>%
      filter((male_never_smoker_min_age > 0 | female_never_smoker_min_age > 0))%>%
      mutate(min_age = pmax (male_never_smoker_min_age, female_never_smoker_min_age), 
             max_age = pmax (male_never_smoker_max_age, female_never_smoker_max_age), 
             uptake = pmax(male_never_smoker_uptake, female_never_smoker_uptake),
             freq = pmax(male_never_smoker_frequency, female_never_smoker_frequency), 
             pack_yrs = 0, quitting_t = 0)
  }
  
  if(agg_by_seed){
    proj_sb = proj_sb %>%
      group_by(Strategy, min_age, max_age, pack_yrs, quitting_t, uptake, freq, screen_gender, Smoking_ban_policy) %>%
      summarise(
        across(
          all_of(metric_vars),
          list(
            median = ~median(.x, na.rm = TRUE),
            l    = ~as.numeric(quantile(.x, 0.025, na.rm = TRUE)),
            u    = ~as.numeric(quantile(.x, 0.975, na.rm = TRUE))
          ),
          .names = "{.col}_{.fn}"
        ),
        .groups = "drop"
      ) %>%
      rename_with(
        ~sub("_median$", "", .x),
        ends_with("_median")
      )
  }
  if(scale){
      if(scale_all){
        proj_sb = proj_sb # %>%group_by(screen_gender)
      }else{
        proj_sb = proj_sb %>%group_by(screen_gender, Smoking_ban_policy)
      }
      if(scale_approach=='lognormal'){
        var_scale = c('ICER', 'QALY', 'Cost_Savings', 'Deaths_Averted','LSA', 'OD_rate', 'FP_rate')
        proj_sb = proj_sb%>%
          mutate(across(all_of(var_scale), 
                        list(
                          logscale = ~log(pmax(.x+1, 1))
                        ),
                        .names = "{.col}_scaled"))%>%
          mutate(
            ICER_scaled           = 1 - (ICER_scaled - min(ICER_scaled)) / (max(ICER_scaled) - min(ICER_scaled)),
            QALY_scaled           = (QALY_scaled - min(QALY_scaled)) / (max(QALY_scaled) - min(QALY_scaled)),
            Cost_Savings_scaled   = 1 - (Cost_Savings_scaled - min(Cost_Savings_scaled)) / (max(Cost_Savings_scaled) - min(Cost_Savings_scaled)),
            Deaths_Averted_scaled = (Deaths_Averted_scaled - min(Deaths_Averted_scaled)) / (max(Deaths_Averted_scaled) - min(Deaths_Averted_scaled)),
            LSA_scaled            = (LSA_scaled - min(LSA_scaled)) / (max(LSA_scaled) - min(LSA_scaled)),
            OD_rate_scaled        = 1 - (OD_rate_scaled - min(OD_rate_scaled)) / (max(OD_rate_scaled) - min(OD_rate_scaled)),
            FP_rate_scaled        = 1 - (FP_rate_scaled - min(FP_rate_scaled)) / (max(FP_rate_scaled) - min(FP_rate_scaled))
          )%>%
          ungroup
      }else if(scale_approach=='standardise'){
        #standardisation
        proj_sb = proj_sb%>%
          mutate(
            ICER_scaled           = - scale(ICER)%>%as.vector(),
            QALY_scaled           = scale(QALY)%>%as.vector(),
            Cost_Savings_scaled   = - scale(Cost_Savings)%>%as.vector(),
            Deaths_Averted_scaled = scale(Deaths_Averted)%>%as.vector(),
            LSA_scaled            = scale(LSA)%>%as.vector(),
            OD_rate_scaled        = - scale(OD_rate)%>%as.vector(),
            FP_rate_scaled        = - scale(FP_rate)%>%as.vector()
          )%>%
          ungroup      
      }else{ #normal
        proj_sb = proj_sb%>%
          mutate(
            ICER_scaled           = 1 - (ICER - min(ICER)) / (max(ICER) - min(ICER)),
            QALY_scaled           = (QALY - min(QALY)) / (max(QALY) - min(QALY)),
            Cost_Savings_scaled   = 1 - (Cost_Savings - min(Cost_Savings)) / (max(Cost_Savings) - min(Cost_Savings)),
            Deaths_Averted_scaled = (Deaths_Averted - min(Deaths_Averted)) / (max(Deaths_Averted) - min(Deaths_Averted)),
            # OverDiagnosis_scaled  = 1 - (OverDiagnosis - min(OverDiagnosis)) / (max(OverDiagnosis) - min(OverDiagnosis)),
            LSA_scaled            = (LSA - min(LSA)) / (max(LSA) - min(LSA)),
            OD_rate_scaled        = 1 - (OD_rate - min(OD_rate)) / (max(OD_rate) - min(OD_rate)),
            FP_rate_scaled        = 1 - (FP_rate - min(FP_rate)) / (max(FP_rate) - min(FP_rate))
          )%>%
          ungroup      
      }
      
    }
  proj_sb
}

#pairwise comparison: probability of one item (column) larger than the other across different simulations(row)
larger_prob = function(mat, summ = F){
  n = ncol(mat)
  # p_ij: prob(i>j) across simulations
  p = mclapply(1:n, function(x1){
    res = lapply(1:n, function(x2){
      ifelse(x1 == x2, NA, mean(mat[, x1] > mat[,x2]))
    })%>%unlist
  }, mc.cores = detectCores())%>%do.call('rbind',.)%>%as.matrix()
  if(summ){
    rowMeans(p, na.rm=T) #summary score: average probability of larger than other items
  }else{
    p
  }
}

#pairwise comparison: t-test<<< whether i>j
pair_ttest = function(mat, summ = F){
  n = ncol(mat)
  # p_ij: prob(i>j) across simulations
  p1 = mclapply(1:n, function(x1){
    res = lapply(1:n, function(x2){
      ifelse(x1 == x2, NA, t.test(mat[, x1] - mat[,x2], alternative = 'greater')$p.value<0.05)
    })%>%unlist
  }, mc.cores = detectCores())%>%do.call('rbind',.)%>%as.matrix()
  p2 = mclapply(1:n, function(x1){
    res = lapply(1:n, function(x2){
      ifelse(x1 == x2, NA, t.test(mat[, x1] - mat[,x2], alternative = 'less')$p.value<0.05)
    })%>%unlist
  }, mc.cores = detectCores())%>%do.call('rbind',.)%>%as.matrix()
  if(summ){
    cbind(rowMeans(p1, na.rm=T), rowMeans(p2, na.rm=T)) #summary score: average probability of larger than other items
  }else{
    list(p1, p2)
  }
}

# lapply(scenario_proj, function(m){
#   lapply(0:1, function(i){
#     df = proj_sb0 %>% filter(screen_gender == i, Smoking_ban_policy == m)
#     find_optim(df, 'OD_rate', min = T, strategy_idx = 1:135+135*i)
#   })%>%unlist
# })%>%do.call('cbind',.)

find_optim = function(df, var0, min = F, strategy_idx = 1:135){
  if(min) df[[var0]] = -df[[var0]]
  x =into_mat(df, var0)%>%
    # larger_prob(summ = T)
    pair_ttest(summ = T) 
  x1 = which(x[,1] == max(x[,1]))
  if(length(x1)>1){
    x2 = which(x[x1, 2] == min(x[x1, 2]))
    x1 = x1[x2]
  }        
  if(length(x1)==1){
    x2 = x1
  }else{
    x2 = lapply(strategy_idx[x1], function(j){
      df%>%filter(Strategy==j)%>%pull(!!as.symbol(var0))%>%mean
    })%>%unlist%>%which.max
    x2 = x1[x2]
  }
  strategy_idx[x2]
}

find_optim_mat = function(mat, min = F, strategy_idx = 1:135){
  if(min) mat = - mat
  x = mat%>%pair_ttest(summ = T) 
  x1 = which(x[,1] == max(x[,1]))
  if(length(x1)>1){
    x2 = which(x[x1, 2] == min(x[x1, 2]))
    x1 = x1[x2]
  }        
  if(length(x1)==1){
    x2 = x1
  }else{
    x2 = lapply(x1, function(j){
      mat[, j]%>%mean
    })%>%unlist%>%which.max
    x2 = x1[x2]
  }
  strategy_idx[x2]
}

#arrange the value of a specific metric (var0) by simulation seed (row) and strategy (column)
into_mat = function(df, var0, var_idx = 'Seed', n.sim = NULL){
  if(is.null(n.sim)) n.sim = max(df[[var_idx]])
  mclapply(1:n.sim, function(i){
    df%>%filter(!!as.symbol(var_idx) == i)%>%pull(!!as.symbol(var0))
  }, mc.cores=max(1, detectCores()-2))%>%
    do.call('rbind',.)%>%
    as.matrix
}

#cumulative density into matrix ready for plotting
cumdf_to_plot_mat = function(dat, x_idx = 1, y_idx = 2:4){
  n.x = nrow(dat)
  y_idx = y_idx[y_idx <= ncol(dat)]
  x = c(rbind(dat[-n.x, x_idx], dat[-1, x_idx]))
  ys = lapply(y_idx, function(i){
    rep(dat[-n.x, i], each = 2)
  })%>%do.call('cbind', .)%>%as.matrix()
  list(x = x, y = ys)
}

#plot functions------------
#scatter
jitter.kernel <- function(X, w=1){
  X_density <- density(X)
  
  Y <- numeric(length(X))
  for(i in seq_along(X)){
    Y[i] <- sum(dnorm(X[i],X,X_density$bw))
  }
  Y <- Y/max(Y)
  
  y <- runif(length(X),-Y*w, Y*w)
  y
}
#atuomatic range setting
range.scaleup <- function(x_rg){
  x_rg+c(-1,1)*.05*(diff(x_rg))
}
#atuomatic range setting (another method)
scale_up = function(range_val, abs_scale = T, delta = 0.05, prop = 0.1){
  out = rep(0, 2)
  if(abs_scale){
    out[1] = round((range_val[1]-delta)*10)/10
    out[2] = round((range_val[2]+delta)*10)/10
  }else{
    
    if(range_val[1]<0){
      out[1] = range_val[1] * (1+prop)
    }else{
      out[1] = range_val[1] * (1-prop)
    }
    if(range_val[2]>0){
      out[2] = range_val[2] * (1+prop)
    }else{
      out[2] = range_val[2] * (1-prop)
    }
  }
  out
}

#xlabel for ratios
find_xtk = function(xrange){
  x1 = ceiling(xrange[1]*10)/10
  x2 = floor(xrange[2]*10)/10
  delta = x2 - x1
  if(delta==0){
    seq(-0.4, 0.4, 0.4)/10 + x1
  }else if(delta<0.2){
    seq(x1, x2, 0.05)
  }else if(delta<=0.4){
    seq(x1, x2, 0.1)
  }else if(delta<=0.8){
    seq(x1, x2, 0.2)
  }else if(delta<=1.2){
    seq(x1, x2, 0.3)
  }else if(delta<=1.6){
    seq(x1, x2, 0.4)
  }else{
    seq(x1, x2, 0.5)
  }
}

#with ci: list object, with one item per list; without ci: matrix, with one item per row
plot_lines=function(plot_data, x_idx=NULL, ci=T,
                    #y_axis_right=F,closer_y=0, 
                    col=clr, line_alpha=0.8, ci_alpha=0.2){
  if(ci){
    n.lines=length(plot_data)
    if(is.null(x_idx)) x_idx=1:nrow(plot_data[[1]])+xrange[1]-1
  }else{
    n.lines=nrow(plot_data)
    if(is.null(x_idx)) x_idx=1:ncol(plot_data)+xrange[1]-1
  }
  pushViewport(plotViewport(margin, xscale=xrange, yscale=yrange, clip=T))
  if(ci){
    for(i in 1:n.lines)
    {
      pred=plot_data[[i]]
      grid.polygon(c(x_idx,rev(x_idx)), c(pred[,2], rev(pred[,3])), 
                   default.units = 'native',
                   gp=gpar(col=scales::alpha(col[i],0), fill=scales::alpha(col[i],ci_alpha)))
    }
  }
  for(i in 1:n.lines){
    if(ci){
      pred=plot_data[[i]][,1]
    }else{
      pred=plot_data[i,]
    }
    grid.lines(x_idx, pred, default.units='native', 
               gp=gpar(col=scales::alpha(col[i],line_alpha), lwd=2))
  }
  popViewport()
  # if(y_axis_right){
  #   pushViewport(plotViewport(margin, xscale=xrange, yscale=yrange, clip=F))
  #   grid.lines(c(1,1,0),c(0,1,1))
  #   grid.yaxis(main=F)
  #   grid.text(y_text,unit(1,'npc')+unit(3-closer_y,'lines'),rot=270)
  #   popViewport()
  # }
}

#one list per category (color)
plot_linedots=function(plot_data,  x_idx=NULL, ci=T, linkpoints=F, link_lty=2, 
                       point_alpha=0.8, line_alpha=0.8, link_alpha=0.6, line_wd=2,
                       value_at_y = T, 
                       scatter = F, scatter_para = 0.3, 
                       # y_axis_right=F, closer_y=0,
                       col=clr, point_style=19, cex=0.5){
  pushViewport(plotViewport(margin, xscale=xrange, yscale=yrange, clip=T))
  for(i in 1:length(plot_data)){
    if(ci){
      n.lines=nrow(plot_data[[i]])
    }else{
      n.lines=length(plot_data[[i]])
    }
    if(is.null(x_idx)) x_idx=1:n.lines+xrange[1]-1
    if(ci){
      x_dot = plot_data[[i]][,1]
    }else{
      x_dot = plot_data[[i]]
    }
    if(scatter){
      x_idx <- x_idx + jitter.kernel(x_dot,scatter_para)
    }
    if(ci){
      for(k in 1:n.lines){
        if(value_at_y){
          x_val = x_idx[k]; y_val = plot_data[[i]][k,2:3]
        }else{
          y_val = x_idx[k]; x_val = plot_data[[i]][k,2:3]
        }
        grid.lines(x_val, y_val, default.units='native', 
                   gp=gpar(col=scales::alpha(col[i],line_alpha), lwd=line_wd))
      }
    }
    if(value_at_y){
      x_val = x_idx; y_val = x_dot
    }else{
      y_val = x_idx; x_val = x_dot
    }
    grid.points(x_val, y_val, default.units='native', pch=point_style[min(i, length(point_style))],
                gp=gpar(col=scales::alpha(col[i],point_alpha), cex=cex))
    if(linkpoints) grid.lines(x_val, y_val, default.units='native', 
                              gp=gpar(col=scales::alpha(col[i],link_alpha), lty=link_lty))
  }
  popViewport()
  # if(y_axis_right){
  #   pushViewport(plotViewport(margin, xscale=xrange, yscale=yrange, clip=F))
  #   grid.lines(c(1,1,0),c(0,1,1))
  #   grid.yaxis(main=F)
  #   grid.text(y_text,unit(1,'npc')+unit(3-closer_y,'lines'),rot=270)
  #   popViewport()
  # }
}

#rectangles for heatmaps (plotting a matrix), one color per column
plot_rect=function(plot_data, row_name = NA, row_name2 = NA, ytk2 = NA, col_sub = 1, rearrange = F,
                   shade_alpha = 0.6, col = clr, line_clr = 'grey', line_wd = 1, 
                   textsize = 0.8, label = T, col_name = NA){
  pushViewport(plotViewport(margin, xscale=xrange, yscale=yrange))
    for(i in 1:n.rows){
      for(j in 1:n.cats){
        if(col_sub==1 & !is.list(plot_data)){
          grid.rect(j, n.rows - i + 0.1, width = 1, height = plot_data[i,j] * 0.8, just = c('right','bottom'), default.units = 'native', 
                    gp = gpar(col = scales::alpha(clr[j], 0), fill = scales::alpha(clr[j], shade_alpha)))
        }else{
          k_tot = lapply(plot_data, function(dat){
            nrow(dat)>0
          })%>%unlist%>%sum()
          tot_col = ifelse(rearrange, k_tot, col_sub)
          k0 = 0
          for(k in 1:col_sub){
            if(nrow(plot_data[[k]])>0){
              k0 = ifelse(rearrange, k0+1, k)
              grid.rect(j - 1 + (k0-1)/tot_col, n.rows - i + 0.1, width = 1/tot_col, height = plot_data[[k]][i,j] * 0.8, 
                        just = c('left','bottom'), default.units = 'native', 
                        gp = gpar(col = 'white', fill = scales::alpha(clr[j], shade_alpha), lwd = 0.5))
            }
          }
        }
        
      }
      if(label & mean(is.na(row_name))==0){
        grid.text(row_name[i], unit(-0.5/textsize, 'lines'), n.rows + 0.5 -i, just='right', default.units = 'native',
                  gp=gpar(cex = textsize))
      }
    }
  if(mean(is.na(row_name2))==0 && mean(is.na(ytk2))==0){
    n.ys=length(row_name2)
    ytk = lapply(0:n.ys, function(m){
      (ytk2[m]+ytk2[m+1])/2
    })%>%unlist
    for(m in 1:n.ys){
      x.pos = unit(-3, 'lines')
      grid.text(row_name2[m], x.pos, ytk[m], default.units = 'native', rot = 90)
      y0 = (ytk[m] - ytk2[m])*0.5
      grid.lines(x.pos, ytk2[m] + c(0, y0) + 0.5, default.units = 'native')
      grid.lines(x.pos, ytk2[m+1] - c(0, y0) - 0.5, default.units = 'native')
    }
  }
    #border
    for(i in 1:n.rows){
      for(j in 1:n.cats){
        grid.rect(j-1, n.rows-i+0.1, width=1, height = 0.8, just=c('left', 'bottom'),
                  default.units = 'native', 
                  gp=gpar(col = line_clr, fill = NA, lwd = line_wd))
      }
    }
    if(!is.na(col_name)) grid.text(col_name, 0, y=unit(1,'npc')+unit(0.2,'lines'),
                                     just=c('left','bottom'),gp=gpar(font=2))
    # for(i in (0: n.rows)){
    #   grid.lines(xrange, i, default.units = 'native', 
    #              gp = gpar(col = line_clr, lwd = line_wd))
    # }
    # for(i in 0: n.cats){
    #   grid.lines(i, yrange, default.units = 'native', 
    #             gp = gpar(col = line_clr, lwd = line_wd))
    # }
  popViewport()
}

#bar plot: plotting a list, whose length is the number of categories, which is also the number of columns for colors
plot_bar=function(plot_data, shade_alpha = 0.6, col = clr, width_between = 0.5, same_col = F){
  if(!is.list(plot_data)) plot_data = list(plot_data)
  n1 = length(plot_data)
  if(n1==1) col = matrix(col, ncol=1)
  n2 = length(plot_data[[1]])
  xrange = c(0, n2)
  pushViewport(plotViewport(margin, xscale=xrange, yscale=yrange))
  for(i in 1:n2){
    w = (1 - width_between)/n1
    for(j in 1:n1){
      x.pos = i - 1 + (j - 1) * w + width_between/2
      if(same_col){
        clr0 = col[i]
      }else{
        clr0 = col[i,j]
      }
      # clr0 = ifelse(same_col, col[i], col[i,j])
      grid.rect(x.pos, 0, width = w, height = plot_data[[j]][i], just = c('left','bottom'), default.units = 'native', 
                gp = gpar(col = 'white', fill = scales::alpha(clr0, shade_alpha)))
    }
  }
  popViewport()
}


plot_xy=function(xmargin=T, ymargin=T, plot_label=NULL, plot_label_above = F, 
                 x_name=NA,y_name=NA, x_name_pos = 0.2, y_name_pos = 3.5, rot_y = T, 
                 x_name_size = 1, y_name_size = 1, x_text_size = 1, y_text_size = 1,
                 closer_y=0, closer_x=0, plot_y=T, customize_yaxis=F, plot_x = T, customize_xaxis=F, auto_xaxis = F,
                 lab_size_x = 1, lab_size_y = 1, lab_pos_x = 0.5, lab_pos_y = 0.2,
                group = NULL, group_at_x = T, group_pos = NULL, group_lab_pos =3, 
                 h_ref = NA, v_ref = NA, ref_col = 'grey90', ref_linetype = 2, ref_linewd = 1.5){
  if(!exists('xrange')) xrange = c(0,1)
  if(!exists('yrange')) yrange = c(0,1)
  pushViewport(plotViewport(margin, xscale=xrange, yscale=yrange))
  #topleft: e.g., a, b
  if(!is.null(plot_label)){
    if(plot_label_above>0){
      grid.text(plot_label, unit(-margin[2],'lines') ,
                unit(1,'npc')+unit(plot_label_above,'lines'),just='left')
    }else{
      grid.text(plot_label, unit(0.5,'lines') ,
                unit(1,'npc')-unit(0.5,'lines'),just='left')
    }
  } 
  #top: column label
  if(!is.na(x_name)) grid.text(x_name, y=unit(1,'npc')+unit(x_name_pos,'lines'),
                               just='bottom',gp=gpar(font=2,cex = y_name_size))
  #left: row label
  if(!is.na(y_name)) grid.text(y_name, x=-unit(y_name_pos,'lines'),gp=gpar(font=2, cex = y_name_size),rot = 90 * rot_y)
  if(!is.na(h_ref)) grid.lines(unit(c(0,1), 'npc'), unit(h_ref, 'native'), 
                                 gp = gpar(col = ref_col, lty = ref_linetype, lwd = ref_linewd))
  if(!is.na(v_ref)) grid.lines(unit(v_ref, 'native'), unit(c(0,1), 'npc'),
                                 gp = gpar(col = ref_col, lty = ref_linetype, lwd = ref_linewd))
  
  if(plot_y){
    grid.lines(c(0,0), c(0, 1))
    if(customize_yaxis){
      grid.yaxis(at=ytk, label=F)
      grid.text(ylab, y=ytk1, x=-unit(lab_pos_y/lab_size_y, 'lines'), just = 'right', 
                default.units = 'native', gp = gpar(cex = lab_size_y))
    }else{
      grid.yaxis()
    }
  }
  if(plot_x){
    grid.lines(c(1,0),c(0,0))
    if(customize_xaxis){
      grid.xaxis(at=xtk, label=F)
      grid.text(xlab, x=xtk1, y=-unit(lab_pos_x/lab_size_x, 'lines'), default.units = 'native', 
                gp = gpar(cex = lab_size_x))
    }else if(auto_xaxis){
      grid.xaxis()
    }else{
      grid.xaxis(at=xtk, label=xlab)
    }
  }
  if(xmargin){
    y.pos=-unit(2.5,'lines')
    grid.text(x_text,y=-unit(2.7-closer_x,'lines')/x_text_size, gp = gpar(cex = x_text_size))
  }
  if(ymargin) grid.text(y_text, x=-unit(3-closer_y, 'lines')/y_text_size,rot=90, gp = gpar(cex = y_text_size))
  
  if(mean(is.null(group))==0 && mean(is.null(group_pos))==0){
    n_g=length(group)
    gtk = lapply(0:n_g, function(m){
      (group_pos[m]+group_pos[m+1])/2
    })%>%unlist
    for(m in 1:n_g){
      x.pos = unit(-group_lab_pos, 'lines')
      if(group_at_x){
        x.pos = gtk[m]; y.pos = unit(-group_lab_pos, 'lines')
      }else{
        x.pos = unit(-group_lab_pos, 'lines'); y.pos = gtk[m]
      }
      grid.text(group[m], x.pos, y.pos, default.units = 'native', rot = 90)
      delta = (gtk[m] - group_pos[m])*0.5
      if(group_at_x){
        x.pos = group_pos[m] + c(0, delta) + 0.5
      }else{
        y.pos = group_pos[m] + c(0, delta) + 0.5
      }
      grid.lines(x.pos, y.pos, default.units = 'native')
      if(group_at_x){
        x.pos = group_pos[m+1] - c(0, delta) - 0.5
      }else{
        y.pos = group_pos[m+1] - c(0, delta) - 0.5
      }
      grid.lines(x.pos, y.pos, default.units = 'native')
    }
  }
  popViewport()
}

plot_legend=function(names,col=clr,point=F,topright=F, topright_pos=10, n.onlyshade=0, 
                     n.lines=0, line_type = 1, right_pos = 0.5, line_length = 1,
                     n.pointlines=0, n.points=0, point_style=19, cex=0.5, shade_alpha = 0.3){
  if(!exists('xrange')) xrange = c(0,1)
  if(!exists('yrange')) yrange = c(0,1)
  pushViewport(plotViewport(margin, xscale=xrange, yscale=yrange))
  n.p=length(names)
  for(i in 1:n.p)
  {
    if(topright){
      x.pos=unit(1,'npc')-unit(topright_pos,'lines')
      y.pos=unit(1,'npc')-unit(i-0.5,'lines')
      x.pos1=unit(1,'npc')-unit(topright_pos-line_length/2,'lines')/cex
      y.pos1=unit(1,'npc')-unit(i-0.5,'lines')/cex
    }else{
      x.pos=unit(1,'npc')+unit(right_pos,'lines')
      y.pos=unit(0.5,'npc')-unit(i-(n.p+1)/2,'lines')*1.5
      x.pos1=unit(1,'npc')+unit(right_pos+line_length/2,'lines')/cex
      y.pos1=unit(0.5,'npc')-unit(i-(n.p+1)/2,'lines')*1.5/cex
    }
    
    if(i<=(n.p-n.points)){
      if(i<=(n.p-n.points-n.pointlines-n.lines)){
        grid.rect(x.pos,y.pos, unit(line_length,'lines'),unit(0.5,'lines'), just='left',
                  gp=gpar(col=scales::alpha(col[i],0),fill=scales::alpha(col[i],shade_alpha)))
      }
      if(i>n.onlyshade) 
        grid.lines(x.pos+unit(c(0,line_length), 'lines'), y.pos,
                   gp=gpar(col=scales::alpha(col[i],0.8),lwd=2, lty = line_type[min(i-n.onlyshade, length(line_type))]))
    }
    if(i>(n.p-n.points-n.pointlines)){
      grid.points(x.pos1, y.pos1, 
                  pch=point_style[min(i-(n.p-n.points-n.pointlines), length(point_style))],
                  gp=gpar(col=scales::alpha(col[i],0.8),cex=cex))
    }
    grid.text(names[i],x.pos+unit(line_length + 0.3, 'lines'), y.pos,just='left')
  }
  popViewport()
}

plot_radar = function(plot_data, col=clr, grid_col='grey90', ngrid = 4, label = NULL, plot_name = NULL, plot_name_bold = T, 
                      shade = F, shade_alpha = 0.2, line_wd = 2, line_alpha = 0.8, line_type = 1, size = 0.8, fontsize = 0.8){
  if(!is.null(plot_name)) grid.text(plot_name, 0, 1, just=c('left','bottom'),gp=gpar(font=1 + plot_name_bold))
  n.lines=length(col)
  size = size / 2
  if(n.lines==1) plot_data=matrix(plot_data, nrow=1)
  n.cat=ncol(plot_data)
  pushViewport(plotViewport(margin, xscale=c(0,1), yscale=c(0,1)))
  theta = seq(0, 2*pi, length.out = n.cat + 1)
  #grid levels
  for(i in 1: ngrid){
    rr <- i / ngrid * size 
    theta0 = seq(0, 2*pi, length.out = 1e3)
    grid.polygon(x = 0.5 + rr * sin(theta0), y = 0.5 + rr * cos(theta0),
                 gp = gpar(col = grid_col, fill = NA))
    # grid.circle(0.5, 0.5, rr, gp = gpar(col = grid_col, fill = NA))
  }
  for(i in 1:n.cat){
    grid.lines(c(0.5, 0.5 + size * sin(theta[i])), c(0.5, 0.5 + size * cos(theta[i])), 
               gp = gpar(col = grid_col))
  }
  if(any(shade)){
    for(k in 1: n.lines){
      if(shade[min(k, length(shade))]){
        dat0 = c(plot_data[k,], plot_data[k,1])
        x = 0.5 + size * dat0 * sin (theta)
        y = 0.5 + size * dat0 * cos (theta)
        grid.polygon(x, y, gp = gpar(col = NA, fill = scales::alpha(col[k], shade_alpha[min(k, length(shade_alpha))])))
      }
    }
  }
  for(k in 1: n.lines){
    dat0 = c(plot_data[k,], plot_data[k,1])
    x = 0.5 + size * dat0 * sin (theta)
    y = 0.5 + size * dat0 * cos (theta)
    grid.lines(x, y, gp=gpar(col = scales::alpha(col[k], line_alpha[min(k, length(line_alpha))]),
                             lwd = line_wd[min(k, length(line_wd))],
                             lty = line_type[min(k, length(line_type))]))
  }
  if(!is.null(label)){
    size1 = min(size + 0.02, 0.5)
    for(i in 1:n.cat){
      x = 0.5 + size1 * sin(theta[i])
      y = 0.5 + size1 * cos(theta[i])
      pos1 = case_when(
        x < 0.5 ~ 'right', 
        x == 0.5 ~ 'center',
        x > 0.5 ~ 'left'
      )
      pos2 = case_when(
        y < 0.5 ~ 'top', 
        y == 0.5 ~ 'center',
        y > 0.5 ~ 'bottom'
      )
      grid.text(label[i], x, y, just = c(pos1, pos2), gp = gpar(cex = fontsize))
    }
  }
  popViewport()
}

plot_strategy = function(strategy_dat, col = clr, point_style = 15:19, point_size = 0.8, 
                         point_alpha = 0.8, point_space = 0.05, shade_clr = 'grey90'){
  n.strategy = length(strategy_dat)
  if(!exists('strategy_name')) strategy_name = name(strategy_dat)
  n.selected = lapply(1:n.strategy, function(k){
    selected_strategy[[k]]%>%length
  })%>%unlist
  n.lines = sum(n.selected) + n.strategy
  pushViewport(plotViewport(margin, xscale=c(0,1), yscale=c(0,n.lines)))
  for(k in 1:n.strategy){
    text.pos = unit(0.3, 'lines')
    y.pos0 = n.lines-sum(n.selected[1:k]+1)
    y.pos1 = y.pos0 + n.selected[k] + 0.5
    grid.rect(0, y.pos0, width = 1, height = n.selected[k] + 1, 
              just = c('left', 'bottom'), default.units = 'native')
    grid.text(strategy_name[k], text.pos, y.pos1, just = 'left',
              default.units = 'native', gp = gpar(font = 2))
    for(i in 1:n.selected[k]){
      y.pos1 = y.pos1 - 1
      if(i%%2 == 1){
        grid.rect(0, y.pos1, width = 1, height = 1,
                  just = c('left'), default.units = 'native',
                  gp = gpar(col = NA, fill = shade_clr))
      }
      dat1 = strategy_dat[[k]][[i]]
      grid.text(dat1$name[1], text.pos, y.pos1, just = 'left',
                default.units = 'native')
      for(j in 1: nrow(dat1)){
        point.pos = 0.5 + point_space * (j-1) 
        s.idx = which(scenario_name == dat1$scenario[j]); g.idx = dat1$gender[j] + 1
        grid.points(point.pos, y.pos1, default.units = 'native',  pch = point_style[g.idx],
                    gp = gpar(col = scales::alpha(col[ifelse(length(col)>n.scenario, s.idx + (g.idx - 1) * n.scenario, s.idx)], point_alpha), cex = point_size))
      }
    }
  }
  grid.lines(c(1,0,0,1,1), c(0,0,1,1,0))
  popViewport()
}

plot_strategy2 = function(strategy_dat, col = clr, shade_alpha = 0.8, scenario_name0 = scenario_name,
                          col_border = 'grey80', row_height = 0.8, xlab_size = 0.75){
  row_height = row_height/2
  n.strategy = length(strategy_dat)
  n.scenario = length(scenario_name0)
  if(!exists('strategy_name')) strategy_name = name(strategy_dat)
  n.selected = lapply(1:n.strategy, function(k){
    selected_strategy[[k]]%>%length
  })%>%unlist
  n.lines = sum(n.selected) + n.strategy
  pushViewport(plotViewport(margin, xscale=c(0,n.scenario), yscale=c(0,n.lines)))
  for(k in 1:n.strategy){
    text.pos = unit(-0.3, 'lines')
    y.pos0 = n.lines-sum(n.selected[1:k]+1)
    y.pos1 = y.pos0 + n.selected[k] + 0.3
    grid.rect(unit(-margin[2], 'lines'), y.pos0, 
              width = unit(1, 'npc') + unit(margin[2], 'lines'), height = n.selected[k] + 1,
              just = c('left', 'bottom'), default.units = 'native', 
              gp = gpar(fill = scales::alpha('grey95', k%%2)))
    grid.text(strategy_name[k], text.pos, y.pos1, just = 'right',
              default.units = 'native', gp = gpar(font = 2))
    for(i in 1:n.selected[k]){
      y.pos1 = y.pos0 + n.selected[k] + 0.5 - i
      
      dat1 = strategy_dat[[k]][[i]]
      grid.text(dat1$name[1], text.pos, y.pos1, just = 'right',
                default.units = 'native')
      for(g in 0:1){
        y.pos2 = y.pos1 + (0.5 - g) * row_height
        for(m in 1:n.scenario){
          alpha_1 = ifelse(with(dat1, sum(scenario == scenario_name0[m] & gender == g)), shade_alpha, 0)
          grid.rect(m, y.pos2, width = 1, height = row_height, 
                    just = 'right', default.units = 'native', 
                    gp = gpar(col = col_border, fill = scales::alpha(col[m, g+1], alpha_1)))
          
        }
        grid.text(c('M','F')[g+1], unit(1, 'npc') + unit(0.2/xlab_size, 'lines'), y.pos2, 
                  just = 'left', default.units = 'native', 
                  gp = gpar(cex = xlab_size))
      }
      
    }
  }
  grid.lines(c(1,1), c(0,1))
  for(i in 1:n.scenario){
    y.pos = unit(-0.3, 'lines')
    grid.text(scenario_name[i], i-0.5, y.pos/xlab_size, 
              just = c('top'), default.units = 'native', 
              gp = gpar(cex = xlab_size))
  }
  popViewport()
}

plot_turnado = function(plot_data, y_lab = NULL, col = clr, shade_alpha = 0.6, 
                        cat_name = NULL, cat_pos = 0.5, y_name = NULL, ylab_pos = 0.2, yname_pos = 3,
                        xrange = c(-1,1), display_val = F, val_round = 0, text_size = 0.8){
  #plot_data is a list of two vectors (side-by-side plot)
  n.lines = length(plot_data[[1]])
  pushViewport(plotViewport(margin, xscale=xrange, yscale=c(0, n.lines)))
  for(i in 1:2){
    if(!is.null(cat_name)) 
      grid.text(cat_name[i], xrange[i]/2, unit(1, 'npc') + unit(cat_pos, 'lines'), 
                default.units = 'native', gp=gpar(font=2))
    for(k in 1:n.lines){
      y.pos = n.lines - k + 0.5
      grid.rect(0, y.pos, 
                width = plot_data[[i]][k], height = 0.8,
                just = c('right', 'left')[i], default.units = 'native', 
                gp = gpar(fill = scales::alpha(col[i], shade_alpha), col = 'white'))
      if(display_val) grid.text(paste0(' ', sprintf(fmt = paste0('%.',val_round,'f'), plot_data[[i]][k]), ' '), 
                                plot_data[[i]][k] * c(-1, 1)[i], y.pos, 
                               just = c('right', 'left')[i], default.units = 'native', 
                               gp = gpar(cex = text_size))
    }
  }
  if(!is.null(y_name))
    grid.text(y_name, unit(-yname_pos, 'lines'), rot = 90)
  if(!is.null(y_lab))
    grid.text(y_lab, unit(-ylab_pos, 'lines'), n.lines : 1 - 0.5,  default.units = 'native', just = 'right')
  popViewport()
}

