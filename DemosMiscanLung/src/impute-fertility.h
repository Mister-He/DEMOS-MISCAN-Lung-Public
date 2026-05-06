#ifndef IMPUTE_FERTILITY_H
#define IMPUTE_FERTILITY_H

#include <RcppArmadillo.h>
#include "Helper.h"

using namespace Rcpp;

// ==================== Function Declarations ====================

int schedule_child(double fertility_rate, int year_now, int yearborn, int race,
                   const NumericVector& fertility_probs);

#endif
