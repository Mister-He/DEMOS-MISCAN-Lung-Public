functions {
  real bell_term(real a, real s) {
    return -square(a - 20) / (2 * square(s));
  }

  real cloglog_prob_safe(real eta) {
    if (eta > 20) return 1.0;
    if (eta < -20) return -expm1(-exp(-20));
    return -expm1(-exp(eta));
  }
}

data {
  int<lower=1> N;
  int<lower=2> n_cp;
  vector[n_cp] cut_points;
  array[N] int<lower=1,upper=3> state_a, state_h;
  array[N, n_cp-1] int<lower=0> soj;

  int<lower=1> K_race;
  array[N] int<lower=0,upper=1> gender;
  array[N] int<lower=1,upper=K_race> race;
  array[N] int<lower=-7,upper=7> yob_group;
}

transformed data {
  int G = 2 * K_race;
}

parameters {
  real theta0;
  real log_tau01;
  vector[G] z01;
  real<lower=log(4), upper=log(6)> log_s01;
  real log_beta_yob01;

  real gamma0;
  real log_gamma1;
  real<lower=0> log_tau12;
  vector[G] z12;
  real log_beta_yob12;
}

transformed parameters {
  real<lower=0> gamma1 = exp(log_gamma1);
  real<lower=0> tau01 = exp(log_tau01);
  real<lower=0> s01 = exp(log_s01);
  real<lower=0> tau12 = exp(log_tau12);
  real<lower=0> beta_yob01 = exp(log_beta_yob01);
  real<lower=0> beta_yob12 = exp(log_beta_yob12);
}

model {
  theta0 ~ normal(log(0.0365), 0.1);
  log_tau01 ~ normal(log(2.88), 0.1);
  log_s01 ~ normal(log(5.67), 0.1);
  log_beta_yob01 ~ normal(log(0.26), 0.5);
  z01 ~ normal(0, 1);

  gamma0 ~ normal(log(0.037), 0.1);
  log_gamma1 ~ normal(log(0.005), 0.5);
  log_tau12 ~ normal(log(2.78), 0.1);
  z12 ~ normal(0, 1);
  log_beta_yob12 ~ normal(log(0.22), 0.5);

  for (i in 1:N) {
    int g = (gender[i] + 1) + 2 * (race[i] - 1);

    real s1 = (state_a[i] == 1);
    real s2 = (state_a[i] == 2);
    real s3 = (state_a[i] == 3);

    for (k in 1:(n_cp - 1)) {
      int dt = soj[i, k];
      if (dt > 0) {
        for (a in 0:(dt - 1)) {
          real age = cut_points[k] + a;

          real eta01 = theta0 + tau01 * z01[g] - beta_yob01 * yob_group[i] + bell_term(age, s01);
          real p01 = cloglog_prob_safe(eta01);

          real eta12 = gamma0 + tau12 * z12[g] + beta_yob12 * yob_group[i] + gamma1 * (age - 40);
          real p12 = cloglog_prob_safe(eta12);

          real ns1 = s1 * (1 - p01);
          real ns2 = s2 * (1 - p12) + s1 * p01;
          real ns3 = s3 + s2 * p12;

          ns1 = fmax(ns1, 0.0);
          ns2 = fmax(ns2, 0.0);
          ns3 = fmax(ns3, 0.0);

          real S = ns1 + ns2 + ns3 + 1e-12;
          s1 = ns1 / S;
          s2 = ns2 / S;
          s3 = ns3 / S;
        }
      }
    }

    real p_obs = (state_h[i] == 1 ? s1 : (state_h[i] == 2 ? s2 : s3));
    target += log(p_obs + 1e-12);
  }
}
