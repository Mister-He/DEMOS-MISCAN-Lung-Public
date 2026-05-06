###############################################################################
# Multi-State Smoking Model: Bayesian Estimation
# Uses Variational Bayes (ADVI) followed by NUTS sampling
###############################################################################

library(dplyr)
library(purrr)
library(rstan)
library(ggplot2)
library(bayesplot)

Sys.setenv(R_MAKEVARS_USER = "~/.R/Makevars_stan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

rm(list = ls())
invisible(gc())

# ========================================
# 1. AGE INTERVALS
# ========================================
create_adaptive_bins <- function() {
  seq(12, 75, by = 1)
}

cut_points <- create_adaptive_bins()
intervals <- tibble(
  lower = head(cut_points, -1),
  upper = tail(cut_points, -1)
)
band_names <- sprintf("soj_%d_%d", intervals$lower, intervals$upper)

# ========================================
# 2. LOAD DATA
# ========================================
dat <- readRDS("data/transition_data_collapsed.rds") %>%
  filter(
    between(age.a, 12, 74),
    between(age.h, 12, 74),
    !(state.a == 2 & state.h == 1)
  )

# ========================================
# 3. COMPUTE SOJOURN TIMES
# ========================================
soj_df <- map2_dfc(intervals$lower, intervals$upper, ~ {
  pmax(0, pmin(dat$age.h, .y) - pmax(dat$age.a, .x))
})
names(soj_df) <- band_names

dat_soj <- bind_cols(dat, soj_df)

# ========================================
# 4. PREPARE COVARIATES
# ========================================
gender <- dat_soj$gender - 1L
yob_group <- pmax((dat_soj$yob - 1970) %/% 5, -7)
race <- dat_soj$race

# ========================================
# 5. STAN DATA
# ========================================
stan_data <- list(
  N          = nrow(dat_soj),
  n_cp       = length(cut_points),
  cut_points = cut_points,
  start_age  = dat_soj$age.a,
  end_age    = dat_soj$age.h,
  gender     = as.integer(gender),
  yob_group  = as.integer(yob_group),
  K_race     = 4L,
  race       = as.integer(race),
  state_a    = as.integer(dat_soj$state.a + 1L),
  state_h    = as.integer(dat_soj$state.h + 1L),
  soj        = as.matrix(select(dat_soj, all_of(band_names)))
)

stopifnot(
  !anyNA(stan_data$soj),
  all(stan_data$soj >= 0),
  all(diff(stan_data$cut_points) > 0)
)

# ========================================
# 6. COMPILE MODEL
# ========================================
mod_safe <- stan_model("piecewise_multistate.stan")

# ========================================
# 7. VARIATIONAL BAYES
# ========================================
fit_vb_fr <- vb(
  object         = mod_safe,
  data           = stan_data,
  algorithm      = "fullrank",
  iter           = 5e4,
  tol_rel_obj    = 1e-6,
  elbo_samples   = 100,
  grad_samples   = 10,
  adapt_engaged  = TRUE,
  adapt_iter     = 1000,
  eta            = 0.05,
  output_samples = 20000,
  seed           = 114514
)

saveRDS(fit_vb_fr, "outputs/fit_vb_fr.rds")
print(fit_vb_fr)

# ========================================
# 8. NUTS SAMPLING
# ========================================
vb_means <- as.list(summary(fit_vb_fr)$summary[, "mean"])
vb_means$lp__ <- NULL

fit_nuts <- sampling(
  object  = mod_safe,
  data    = stan_data,
  init    = function() vb_means,
  chains  = 4,
  iter    = 4000,
  warmup  = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  seed    = 1
)

saveRDS(fit_nuts, "outputs/fit_nuts.rds")
print(fit_nuts)

# ========================================
# 9. DIAGNOSTICS
# ========================================
sampler_params <- get_sampler_params(fit_nuts, inc_warmup = FALSE)

divergences <- sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))
cat("Divergent transitions:", divergences, "\n")

max_depth_hits <- sum(sapply(sampler_params, function(x) sum(x[, "treedepth__"] == 15)))
cat("Max tree depth hits:", max_depth_hits, "\n")

ebfmi_chain <- function(energy) {
  dE <- diff(energy)
  mean(dE^2) / var(energy)
}

ebfmi_vals <- sapply(sampler_params, function(x) ebfmi_chain(x[, "energy__"]))
cat("E-BFMI per chain:", ebfmi_vals, "\n")
cat("Mean E-BFMI:", mean(ebfmi_vals), "\n")

# ========================================
# 10. VISUALIZATION
# ========================================
post <- as.array(fit_nuts)

pars <- c(
  "theta0", "log_tau01",
  "z01[1]", "z01[2]", "z01[3]", "z01[4]", "z01[5]", "z01[6]", "z01[7]", "z01[8]",
  "log_s01", "log_beta_yob01",
  "gamma0", "log_gamma1", "log_tau12",
  "z12[1]", "z12[2]", "z12[3]", "z12[4]", "z12[5]", "z12[6]", "z12[7]", "z12[8]",
  "log_beta_yob12"
)

p1 <- mcmc_dens(post, pars = pars)
ggsave("../../../outputs/figures/SmokingRate/mcmc_density_all.png",
  p1,
  width = 10, height = 8, dpi = 600
)

p2 <- mcmc_dens_overlay(post, pars = pars)
ggsave("../../../outputs/figures/SmokingRate/mcmc_density_overlay.png",
  p2,
  width = 10, height = 8, dpi = 600
)

p3 <- mcmc_trace(post, pars = pars[1:8], n_warmup = 0, facet_args = list(ncol = 4))
p4 <- mcmc_trace(post, pars = pars[9:16], n_warmup = 0, facet_args = list(ncol = 4))
p5 <- mcmc_trace(post, pars = pars[17:24], n_warmup = 0, facet_args = list(ncol = 4))

ggsave("../../../outputs/figures/SmokingRate/mcmc_trace_1_8.png",
  p3,
  width = 10, height = 8, dpi = 600
)
ggsave("../../../outputs/figures/SmokingRate/mcmc_trace_9_16.png",
  p4,
  width = 10, height = 8, dpi = 600
)
ggsave("../../../outputs/figures/SmokingRate/mcmc_trace_17_24.png",
  p5,
  width = 10, height = 8, dpi = 600
)

# ========================================
# 11. EXPORT POSTERIOR SUMMARY
# ========================================
summary_fit <- summary(fit_nuts, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
posterior_summary <- summary_fit$summary[pars, ]

write.csv(posterior_summary, file = "outputs/smoking_params.csv")
write.csv(posterior_summary, file = "../../../params/smoking_params.csv")

###############################################################################
# END OF SCRIPT
###############################################################################
