#ifndef IMPUTE_SMOKING_H
#define IMPUTE_SMOKING_H

#include <RcppArmadillo.h>
#include "Helper.h"

using namespace Rcpp;

// ==================== Function Declarations ====================

double schedule_intensity(int gender, double rng);

void schedule_smoke(PersonData& person, int index, int gender, int yearborn, int age,
                    const NumericVector& smoking_params,
                    const NumericVector& smoking_calibration_params,
                    SMOKING_STATUS& smoking_status, SMOKING_BAN_POLICY& smoking_ban_policy,
                    const NumericVector& smoking_probs);

#endif
