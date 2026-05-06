#ifndef SIMULATION_H
#define SIMULATION_H

#include <RcppArmadillo.h>
#include "Helper.h"

using namespace Rcpp;

// ==================== Function Declarations ====================

NumericVector schedule_individual(const IndividualInfo& individual, const ModelData& model_data,
                                  const DiseaseParams& disease_params,
                                  const SmokingParams& smoking_params,
                                  const SimulationConfig& config, OutputMatrices& output_matrices);

// [[Rcpp::export]]
NumericMatrix schedule_population(
    const int ncol, const int begin, const int end, const int year_stop,
    const NumericVector& smoking_params, const NumericVector& smoking_calibration_params,
    const NumericVector& lc_baseline_risk_params, const NumericMatrix& lc_dwell_weibull_params,
    const NumericMatrix& lc_transition_rates, const NumericVector& lc_survival_params,
    const NumericMatrix& utility, const NumericMatrix& ave_cost, const int screen,
    const NumericVector& strategy_cond, const int trunc_age, const int smoking_ban_policy_index,
    const int screen_mode, const int sensitivity_level, const bool verbose);

#endif  // SIMULATION_H
