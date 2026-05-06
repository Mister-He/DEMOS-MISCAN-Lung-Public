#ifndef IMPUTE_SCREENING_H
#define IMPUTE_SCREENING_H

#include <RcppArmadillo.h>
#include "Helper.h"

using namespace Rcpp;

// ==================== Helper Struct ====================

struct StrategyParams {
  double min_age, max_age, uptake, pack_yrs, quit_period, freq;
  bool family_history_required;

  StrategyParams(double ma, double Ma, double u, double py, double qp, double f, bool fh)
      : min_age(ma),
        max_age(Ma),
        uptake(u),
        pack_yrs(py),
        quit_period(qp),
        freq(f),
        family_history_required(fh) {}
};

// ==================== Function Declarations ====================

void get_sensitivities(SENSITIVITY_LEVEL level, const double*& smoker_sens,
                       const double*& nonsmoker_sens);

void schedule_screening(const int thisyear, int& what_if_dx_yr, PersonData& person,
                        const NumericVector& screen_probs, bool& false_positive,
                        LUNG_CANCER_STATUS& lung_status,
                        SENSITIVITY_LEVEL sensitivity_level);

bool should_screen(const int thisyear, const PersonData& person, const int age,
                   const int gender, const NumericVector& strategy_cond,
                   const NumericVector& screen_probs, const int screen,
                   const int last_screen_year, SENSITIVITY_LEVEL sensitivity_level);

bool should_screen_fh(const int thisyear, const PersonData& person, const int age,
                      const int gender, const NumericVector& strategy_cond,
                      const NumericVector& screen_probs, const int screen,
                      const int last_screen_year, SENSITIVITY_LEVEL sensitivity_level);

#endif  // IMPUTE_SCREENING_H
