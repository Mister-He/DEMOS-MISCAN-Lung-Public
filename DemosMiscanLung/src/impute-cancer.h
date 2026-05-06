#ifndef IMPUTE_CANCER_H
#define IMPUTE_CANCER_H

#include <RcppArmadillo.h>
#include "Helper.h"

using namespace Rcpp;

// ==================== Function Declarations ====================

void schedule_lung_cancer(const int index, const int yearborn, const int yob_group,
                          const int age, const int thisyear, PersonData& person,
                          const NumericVector& lc_probs,
                          const NumericVector& lc_baseline_risk_params,
                          const NumericMatrix& lc_dwell_weibull_param,
                          const NumericMatrix& lc_transition_rates,
                          LUNG_CANCER_STATUS& lung_status);

#endif
