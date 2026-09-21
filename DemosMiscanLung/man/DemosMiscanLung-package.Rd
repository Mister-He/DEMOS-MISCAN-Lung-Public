\name{DemosMiscanLung-package}
\alias{DemosMiscanLung-package}
\alias{DemosMiscanLung}
\docType{package}
\title{Microsimulation Model for Lung Cancer Screening in Singapore}
\description{
Discrete event microsimulation for evaluating lung cancer screening and tobacco
control policies in Singapore. The model represents individual smoking histories,
lung cancer natural history, screening and diagnosis, survival, treatment costs,
and quality-adjusted life years (QALYs). R interfaces call a compiled C++ engine
through Rcpp. The accompanying repository supplies population and parameter
files, simulation launchers, calibration code, and saved-result analyses.
}
\details{
The population model includes births, immigration, and mortality, with
sex-specific and ethnicity-specific inputs. Individual outcomes include smoking
initiation and cessation, smoking intensity, lung cancer onset, stage dwell times,
diagnosis, screening detection, lung cancer death, treatment and screening costs,
QALYs, screening rounds, false positives, and family history.

Screening strategies specify eligibility and uptake separately by sex and smoking
history, including age limits, pack-year thresholds, time since quitting,
screening interval, and family history. Tobacco control scenarios use indices
0 through 4: baseline, mild, moderate, stringent, and immediate ban.

The exported functions \code{runSim()} and \code{schedule_population()} are
low-level interfaces. The former delegates to the latter. Their arguments
include population bounds, the end year, smoking and cancer parameters, utility
and cost arrays, screening settings, age truncation, policy index, family-history
mode, sensitivity level, and verbosity. The repository runner additionally
prepares population vectors and demographic arrays needed by the engine.
Use \file{simulation/main.R} for a complete simulation rather than invoking the
interfaces without this setup.
}
\section{Installation and repository requirements}{
The package imports \pkg{Rcpp} (>= 1.1.0) and links to \pkg{Rcpp} and
\pkg{RcppArmadillo}. Install both before building. R 4.5.3 is the reference
environment used by the repository; the package metadata does not declare a
minimum R version. A C++17-capable R build toolchain is recommended.

From a terminal at the repository root:
\preformatted{R CMD INSTALL --preclean DemosMiscanLung}

The supplied \file{src/libmycode.a} contains macOS ARM64 objects. Its engine
implementations are not all present as source files in this public checkout.
Other operating systems or CPU architectures require a compatible archive or
the engine sources to rebuild it. The archive must also be compatible with the
local R/C++ toolchain.

Repository simulations additionally require \pkg{plyr}, \pkg{dplyr},
\pkg{data.table}, \pkg{tidyr}, \pkg{jsonlite}, and \pkg{MASS}. They use files
outside the installed package, notably \file{params/} and \file{simulation/};
retain the repository checkout. These workflow dependencies and data files are
not automatically installed with the package.
}
\section{Configuration and single runs}{
The runner reads \file{params/config.json}. Defaults select node 1, seed 42,
strategy row 1 (no screening), baseline tobacco control, an end-year setting of
2050, age truncation at 90, screening without family-history mode, and medium
screening sensitivity. The individual outcome matrix has 20 columns.

\file{simulation/load.R} reads \file{params/strategy.csv}. Strategy arguments
are one-based row numbers; the supplied first row has strategy ID 0. Numerical
logs report the row number minus one. Select alternative strategy files in the
loader, and preserve the ID/row convention when constructing new tables.

With no command-line arguments, \file{simulation/main.R} uses the configuration
and fitted parameter estimates. It changes the working directory to
\file{simulation/} and clears its evaluation environment, so use a fresh R
session. A single run uses the full population, including an initial population
of approximately 2.74 million records before immigration and simulated births.

From the repository root, a command-line run takes four arguments:
\preformatted{Rscript simulation/main.R 1 42 1 0}
These are node, seed, strategy row, and zero-based policy index. Supplying
arguments activates parameter sampling for smoking, calibration factors,
cancer risk, survival, and treatment costs, regardless of \code{psa.flag}.
The simulation seed is reset after sampling. Setting \code{psa.flag} to true
additionally saves PSA RDS files for command-line runs; it does not itself
launch repetitions. The current runner does not use \code{psa.samples}.
}
\section{Simulation outputs}{
\itemize{
  \item \code{populationdata}: population characteristics and 20 individual
  outcome columns, available after an interactive run.
  \item \code{result}: total costs, treatment and screening costs, QALYs, life
  years, screening counts, screened-person counts, and false positives.
  \item \code{incidenceindiv} and \code{mortindiv}: annual burden matrices with
  176 columns spanning sex, age group, screening detection, and stage.
  \item \file{outputs/txts/log_node_<node>.txt}: appended numerical summaries.
  \item With PSA output enabled for command-line runs,
  \file{outputs/lc_psa_output/} stores RDS lists with \code{ind_mort},
  \code{Cost}, and \code{QALY}; \file{outputs/smoke_psa_output/} stores
  smoking-prevalence summaries.
}
Create output directories before running. Individual-level records are not
automatically saved. PSA filenames contain seed and policy but omit strategy
and node; multi-strategy runs can overwrite these files. Keep PSA output
disabled for strategy sweeps unless output naming is adapted.
}
\section{Batch simulations}{
\file{simulation/simulation.sh} divides a strategy/seed/policy grid among
workers. From the repository root, a small batch is:
\preformatted{bash simulation/simulation.sh run -n 2 -s 1 -e 2 -st 3 -p 0 -P 0}
This runs six full-population simulations on two workers. The grid mode uses
zero-based policy bounds and inclusive seed bounds. Worker logs and PID files
are under \file{outputs/logs/} and \file{outputs/pids/}. Status reports process
liveness; inspect R logs and result counts to establish success.

The \code{run_single} subcommand must be run from \file{simulation/}; it uses
a one-based policy argument and interprets its seed option as a repetition
count. The \code{run_hpc_node} subcommand supports scheduler-managed partitions.
See the repository README for complete examples and resource considerations.

For repeated no-screening projections, set \code{psa.flag} to true and run
\file{psa.sh} from \file{simulation/}:
\preformatted{bash psa.sh -n 100 -c 5 -p 0}
This uses strategy row 1 and seeds 1 through 100. Its output-directory option
changes launcher tracking paths, not the R simulation output paths.
}
\section{Figures and numerical analysis}{
Saved results are under \file{outputs/DEMOS_LC_result/outs_sim/} in the
repository. They can be analysed without installing the compiled package.
The \file{aggressive/} and \file{conservative/} branches contain analyses with
and without survival extrapolation. Additional directories contain family-history,
pack-year, and screening-sensitivity analyses.

Saved-result workflows use \pkg{dplyr}, \pkg{tidyr}, \pkg{viridis},
\pkg{scales}, \pkg{stringr}, and \pkg{writexl}, together with R's
\pkg{grid} and \pkg{parallel} packages. Use dplyr 1.1.0 or later for
\code{reframe()}. Some helpers use fork-based parallelism and need adaptation
to serial execution on platforms without that support.

\file{outputs/DEMOS_LC_result/code/functions.R} defines data-cleaning, uncertainty,
ranking, and plotting helpers. \file{plot.R} in the same result directory supplies
figure recipes and layout settings; source the helper definitions and run the
relevant data-preparation and figure sections. The plotting helpers depend on
layout variables in their environment. Sourcing the function file alone does
not produce figures. The current figure script requires section selection and
adjustment of its screening-sensitivity input paths.

\file{analysis/derive.R} contains exploratory summaries, Tables S6--S8, and
manuscript numerical comparisons. The opening section requires
\code{populationdata}; later sections read saved result tables. Tables S6--S8
use repository-root-relative paths, while subsequent comparisons use paths
relative to \file{analysis/}. The table blocks share intermediate objects and
default to the same spreadsheet filename. Run them in order with distinct export
names, as demonstrated in the README.

Incremental outcomes compare screening with no screening within the same seed
and tobacco policy. ICER is additional cost divided by QALY gain; OD and FP rates
are percentages. Numerical summaries commonly report medians and the 2.5th and
97.5th percentiles across seeds. Some comparisons pair rows by position, requiring
matching seed order. Preserve this alignment when adapting analyses.

\code{proj_clean()} renames QALY gain to \code{QALY}, total QALYs to
\code{QALY_raw}, and additional cost to \code{Cost_Savings}; positive values of
the latter still represent additional costs. It requires the metric definitions
used in \file{plot.R}. Keep unscaled outcomes for reporting numbers.
}
\author{Yichen He \email{e0732876@u.nus.edu}}
\seealso{
\code{help(package = "DemosMiscanLung")},
\code{\link[Rcpp:evalCpp]{Rcpp::evalCpp}}, and the repository \file{README.md}
for executable figure/table examples and batch command options.
}
\examples{
\dontrun{
# Start a fresh R session at the repository root after installing the package.
dir.create("outputs/txts", recursive = TRUE, showWarnings = FALSE)
source("simulation/main.R")

# The runner leaves the working directory at simulation/.
print(result)
head(populationdata)
dir.create("../outputs/simulations", recursive = TRUE, showWarnings = FALSE)
saveRDS(result, "../outputs/simulations/single_run_summary.rds")
setwd("..")

# Inspect the supplied aggregate results without rerunning the model.
screening <- read.csv(paste0(
  "outputs/DEMOS_LC_result/outs_sim/aggressive/",
  "demos_screening_result_with_smoking_ban_policy_table_",
  "with_survival_extrapolation.csv"
))
mls <- subset(screening, Strategy == 22 & Smoking_ban_policy == "Baseline")
quantile(mls$ICER, c(0.025, 0.5, 0.975))
}
}
\keyword{package}
\keyword{models}
