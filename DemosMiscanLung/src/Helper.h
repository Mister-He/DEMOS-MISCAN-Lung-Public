#ifndef HELPER_FUNCTION_H
#define HELPER_FUNCTION_H

#include <RcppArmadillo.h>
using namespace Rcpp;

// ==================== Status Enums ====================

// Smoking status enum
enum SMOKING_STATUS { NEVER_SMOKER = 0, CURRENT_SMOKER = 1, FORMER_SMOKER = 2 };

// Lung cancer status enum
enum LUNG_CANCER_STATUS { HEALTHY = 0, ONSET = 1, DIAGNOSED = 2, CURED = 3 };

// Smoking ban policy index
enum SMOKING_BAN_POLICY { NO_BAN = 0, MILD = 1, MODERATE = 2, HIGH = 3, IMMEDIATE_CESSATION = 4 };

// Screening sensitivity level
enum SENSITIVITY_LEVEL { SENS_LOW = 0, SENS_MEDIUM = 1, SENS_HIGH = 2 };

// ==================== Person Data Struct ====================

struct PersonData {
  double year_of_death = 0;           // 0: Year of death (0 if alive beyond trunc_age)
  double smoking_start_year = 0;      // 1: Year started smoking
  double smoking_cessation_year = 0;  // 2: Year quit smoking
  double smoking_intensity = 0;       // 3: Pack-years per year
  double year_lc_onset = 0;           // 4: Year lung cancer developed
  double stage1_dwelling_time = 0;    // 5: Years in stage 1
  double stage2_dwelling_time = 0;    // 6: Years in stage 2
  double stage3_dwelling_time = 0;    // 7: Years in stage 3
  double stage4_dwelling_time = 0;    // 8: Years in stage 4
  double year_lc_diagnosis = 0;       // 9: Year clinically diagnosed with LC
  double lc_stage_at_diagnosis = 0;   // 10: Stage at diagnosis (1-4, 0 if undiagnosed)
  double screening_detected = 0;      // 11: Screening detected flag (0 or 1)
  double lc_specific_death_flag = 0;  // 12: LC-specific death flag (0 or 1)
  double treatment_cost = 0;          // 13: Treatment cost (discounted)
  double lc_type = 0;                 // 14: LC type (0=none, 1=NSCLC, 2=SCLC)
  double qalys = 0;                   // 15: QALYs (discounted)
  double times_screened = 0;          // 16: Total screening count
  double screening_cost = 0;          // 17: Screening cost (discounted)
  double false_positive_count = 0;    // 18: False positive count
  double family_history = 0;          // 19: Family history flag (0 or 1)
};

// ==================== Parameter Structs ====================

struct IndividualInfo {
  int ind;
  int index;
  int yearborn;
  int yearimmigrated;
  int length;
};

struct ModelData {
  const arma::cube& fertility_cube;
  const arma::cube& mortality_cube;
  const NumericMatrix& utility;
  const NumericMatrix& ave_cost;
};

struct DiseaseParams {
  const NumericVector& lc_baseline_risk_params;
  const NumericMatrix& lc_dwell_weibull_params;
  const NumericMatrix& lc_transition_rates;
  const NumericVector& lc_survival_params;
};

struct SmokingParams {
  const NumericVector& params;
  const NumericVector& calibration_params;
  SMOKING_BAN_POLICY ban_policy;
};

struct SimulationConfig {
  int year_stop;
  int trunc_age;
  int screen;
  int screen_mode;
  int sensitivity_level;
  const NumericVector& strategy_cond;
};

struct OutputMatrices {
  IntegerMatrix& birthmatrix;
  IntegerMatrix& incidencematrix;
  IntegerMatrix& mortmatrix;
};

// ==================== Helper Function Declarations ====================

int group_cohort(double yearborn);
int group_age_index(const int age);
double cost_calculation(const PersonData& person, const NumericMatrix& ave_cost, const int thisyear);
int stage_locate(const PersonData& person, const int thisyear);
NumericVector persondata_to_vector(const PersonData& p);

// Scenario switching function
// Sensitivity switch

// Screen switch
using ScreenFn = bool (*)(const int, const PersonData&, const int, const int, const NumericVector&,
                          const NumericVector&, const int, const int, SENSITIVITY_LEVEL);

#endif  // HELPER_FUNCTION_H