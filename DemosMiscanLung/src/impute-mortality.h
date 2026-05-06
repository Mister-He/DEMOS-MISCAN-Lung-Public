#ifndef IMPUTE_MORTALITY_H
#define IMPUTE_MORTALITY_H

#include <RcppArmadillo.h>
#include "Helper.h"

using namespace Rcpp;

// ==================== Function Declarations ====================

void schedule_death(const int age, const int thisyear, const int what_if_dx_yr,
                    LUNG_CANCER_STATUS& lung_status, PersonData& person,
                    const double basemortality, const NumericVector& death_probs,
                    const NumericVector& lc_survival_params);

#endif