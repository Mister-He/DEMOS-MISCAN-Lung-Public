#include <RcppArmadillo.h>
#include "simulation.h"

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
NumericMatrix runSim(
    const int ncol, const int begin, const int end, const int year_stop,
    const NumericVector& smoking_params, const NumericVector& smoking_calibration_params,
    const NumericVector& lc_baseline_risk_params, const NumericMatrix& lc_dwell_weibull_params,
    const NumericMatrix& lc_transition_rates, const NumericVector& lc_survival_params,
    const NumericMatrix& utility, const NumericMatrix& ave_cost, const int screen,
    const NumericVector& strategy_cond, const int trunc_age, const int smoking_ban_policy_index,
    const int screen_mode, const int sensitivity_level, const bool verbose) {
  
  return schedule_population(ncol, begin, end, year_stop, smoking_params, 
                             smoking_calibration_params, lc_baseline_risk_params, 
                             lc_dwell_weibull_params, lc_transition_rates, 
                             lc_survival_params, utility, ave_cost, screen, 
                             strategy_cond, trunc_age, smoking_ban_policy_index, 
                             screen_mode, sensitivity_level, verbose);
}
