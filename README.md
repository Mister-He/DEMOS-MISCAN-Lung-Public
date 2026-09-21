# DEMOS-MISCAN-Lung

DEMOS-MISCAN-Lung is a discrete event microsimulation model for evaluating lung cancer screening and tobacco control policies in Singapore. It represents individual smoking histories, lung cancer onset and stage progression, screening and diagnosis, survival, treatment costs, and quality-adjusted life years (QALYs), within a population that includes births, immigration, and mortality. Screening strategies can vary by sex, smoking history, age, uptake, pack-years, time since quitting, screening interval, and family history of lung cancer.

The repository contains the `DemosMiscanLung` R package, simulation scripts, model fitting and calibration code, and saved results used for figures and numerical analyses. There are two entry points:

- **Run new simulations:** install the compiled package and use `simulation/main.R` or the batch launchers.
- **Analyse the supplied results:** use `outputs/DEMOS_LC_result/outs_sim/` directly. This does not require installing the simulation package or rerunning the population model.

## Project structure

```text
DEMOS-MISCAN-Lung-Public/
├── DemosMiscanLung/                 R package
│   ├── R/RcppExports.R             R interfaces: runSim(), schedule_population()
│   ├── src/                        C++ wrapper, headers, and libmycode.a
│   └── man/                        Package help
├── simulation/
│   ├── main.R                      Single simulation and result logging
│   ├── load.R                      Population, parameter, and strategy loading
│   ├── utilis.R                    Population and outcome summarisation helpers
│   ├── simulation.sh               Parallel strategy/seed/policy runs
│   ├── psa.sh                      Repeated baseline runs for uncertainty analysis
│   ├── submit.pbs                  Example cluster submission script
│   └── strategy_design.R           Strategy generation script
├── params/                         Configuration, strategies, fitted parameters
│   ├── config.json
│   ├── strategy.csv                Strategy table actually read by load.R
│   ├── population/                 Initial population, immigration, death, fertility
│   ├── calibration/                Smoking calibration estimates and draws
│   └── hessian/                    Parameter covariance/Hessian files
├── data/                           Calibration targets and reference data
├── modules/                        Smoking, cancer risk, survival, and cost fitting
├── analysis/
│   ├── icer_table_construction.R    Extract logs and construct economic outcomes
│   ├── prev_ind_mort_projection.R  Summarise repeated burden/prevalence runs
│   └── derive.R                    Manuscript tables and numerical comparisons
└── outputs/
    └── DEMOS_LC_result/
        ├── outs_sim/               Saved scenario results: CSV and RDS
        ├── outs/                   Supporting observed and fitted data
        ├── code/functions.R        Cleaning, ranking, and plotting helpers
        ├── plot.R                  Manuscript figure recipes
        └── plots/                  Saved figures
```

Paths below are relative to the repository root unless a working-directory change is shown. Installing the R package does not install the sibling `params/`, `simulation/`, or analysis directories; retain the repository checkout to use these workflows.

## Installation

### Requirements

Use R 4.5.3 as the reference environment, a C++17-capable R build toolchain, and Bash for the batch scripts. `DESCRIPTION` does not declare a minimum R version; compatibility with older R versions is not established here. The analysis uses `dplyr::reframe()`, so use dplyr 1.1.0 or later.

The public package links `DemosMiscanLung/src/libmycode.a` through `src/Makevars`. The supplied archive contains macOS ARM64 objects (Apple Silicon); the underlying engine implementations are not all supplied as C++ source. A compiler alone therefore does not make this checkout portable: Linux, Windows, or Intel macOS installations require a compatible engine archive or the engine sources to rebuild it. Even on Apple Silicon, the archive must be compatible with the local R/C++ toolchain. The saved-result analysis is independent of this binary dependency.

### Install R dependencies

In R, install the packages needed for simulation:

```r
install.packages(c(
  "Rcpp", "RcppArmadillo", "plyr", "dplyr", "data.table",
  "tidyr", "jsonlite", "MASS"
))
```

For saved-result analysis, figure recipes, and spreadsheet exports:

```r
install.packages(c("dplyr", "tidyr", "viridis", "scales", "stringr", "writexl"))
```

`grid` and `parallel` are included with R. Training and calibration scripts have additional dependencies, such as `rstan`, `bayesplot`, `survival`, `flexsurv`, and `mgcv`, and some refer to raw datasets that are not included. They are not prerequisites for using the supplied parameters or saved results.

### Build and load the package

From a terminal at the repository root, on a compatible platform:

```bash
R CMD INSTALL --preclean DemosMiscanLung
```

Then check the installation in R:

```r
library(DemosMiscanLung)
packageVersion("DemosMiscanLung")
help("DemosMiscanLung-package", package = "DemosMiscanLung")
```

The package exports `runSim()` and `schedule_population()`. These are low-level interfaces: the simulation also needs population vectors and demographic arrays prepared by the repository scripts. Use `simulation/main.R` as the starting point for a complete run.

## Run a single simulation

### Configuration

Edit [params/config.json](params/config.json) before starting a run. Its checked-in defaults are:

| Setting | Default | Meaning |
| --- | --- | --- |
| `configuration.num_node` | `1` | Identifier used in the text log filename |
| `configuration.num_seed` | `42` | Simulation random seed |
| `configuration.num_strategy` | `1` | One-based row of `params/strategy.csv` |
| `configuration.screen_mode` | `"without_fh"` | Family-history mode; alternative: `"with_fh"` |
| `configuration.sensitivity_lvl` | `"medium"` | Screening sensitivity: `"low"`, `"medium"`, or `"high"` |
| `configuration.yearstop` | `2050` | Simulation end-year setting |
| `configuration.smoking ban policy index` | `0` | Tobacco control scenario, indexed 0–4 |
| `simulation.start_row` | `0` | Starting population offset; keep 0 for a complete run |
| `simulation.trunc_age` | `90` | Age truncation |
| `simulation.condition_number` | `20` | Number of individual outcome columns; keep 20 |
| `psa.flag` | `false` | Save additional PSA RDS outputs for command-line runs |
| `psa.samples` | `100` | Configuration metadata; the current runner does not use it to launch repetitions |

Strategy row 1 is the no-screening baseline (`ID = 0`). The supplied table has 271 rows: baseline plus 270 screening strategies. The runner indexes **rows**, while logs report `strategy_no - 1` and saved result tables use `Strategy` IDs. For example, saved Strategy 22 corresponds to runner argument 23 in the supplied table. If replacing the strategy table, preserve this ordering convention.

| Policy index | Label in result CSVs | Label in raw PSA filenames |
| --- | --- | --- |
| 0 | `Baseline` | `Baseline` |
| 1 | `Mild (20%)` | `Mild` |
| 2 | `Moderate (50%)` | `Moderate` |
| 3 | `High (80%)` | `Stringent` |
| 4 | `Immediate Ban (100%)` | `Immediate_ban` |

Alternative strategy CSVs under `params/` are not selected automatically. To use one, update the strategy-file selection in `simulation/load.R`. `strategy_design.R` writes separate CSVs; review its active grids and output filenames before running it.

### Run with fixed parameter estimates

Start a fresh R session at the repository root:

```r
dir.create("outputs/txts", recursive = TRUE, showWarnings = FALSE)
source("simulation/main.R")

# main.R changes the working directory to simulation/.
print(result)
head(populationdata)

# Optional: retain the full run for individual-level analysis.
dir.create("../outputs/simulations", recursive = TRUE, showWarnings = FALSE)
saveRDS(
  list(populationdata = populationdata, result = result,
       incidence = incidenceindiv, mortality = mortindiv),
  "../outputs/simulations/baseline_seed_42.rds"
)
setwd("..")
```

`main.R` clears objects in its evaluation environment with `rm(list = ls())`; use a fresh session. With no command-line arguments, it reads the node, seed, strategy, and policy from the JSON and uses the fitted parameter estimates. The filename above assumes the default seed and strategy; change it for other configurations.

This is a single **full-population** run, not a small toy example: the initial population file contains about 2.74 million records, before immigration and simulated births. Runtime and memory needs depend on the machine and scenario. Run one scenario to establish resource requirements before launching many workers.

### Command-line run with parameter sampling

```bash
mkdir -p outputs/txts outputs/lc_psa_output outputs/smoke_psa_output
Rscript simulation/main.R 1 42 1 0
```

The four arguments are `node seed strategy_row policy_index`; supply all four. Whenever arguments are present, `main.R` samples smoking parameters, smoking calibration factors, lung cancer risk parameters, survival parameters, and treatment costs. This happens even when `psa.flag` is `false`. The flag controls additional RDS output, not whether parameters are sampled. The simulation seed is reset after parameter sampling.

### Outputs

- `outputs/txts/log_node_<node>.txt`: appended summaries containing costs, QALYs, life years, screening counts, false positives, cancer cases, and deaths.
- In an interactive session: `populationdata`, `result`, `incidenceindiv`, and `mortindiv`. Individual records include smoking history, cancer onset and diagnosis, stage, screening detection, treatment and screening costs, QALYs, and family history.
- With `psa.flag = true` **and** command-line arguments: `outputs/lc_psa_output/run_seed_<seed>_policy_<label>.rds` and `outputs/smoke_psa_output/run_seed_<seed>_policy_<label>.rds`. The former contains `ind_mort`, `Cost`, and `QALY`; the latter contains flattened smoking-prevalence summaries.

The runner does not automatically save `populationdata`, write a screening-result CSV, or populate `outs_sim/`. Create the output directories before running. PSA filenames omit strategy and node, so different strategies with the same seed and policy overwrite these RDS files; leave `psa.flag = false` for a multi-strategy sweep unless you change the output naming.

## Large-scale simulation runs

### Parallel strategy, seed, and policy sweep

`simulation/simulation.sh` divides the scenario grid across local worker processes. Its “nodes” are worker identifiers; `run` does not allocate remote cluster machines.

From the repository root, first try a small batch:

```bash
mkdir -p outputs/txts
bash simulation/simulation.sh run -n 2 -s 1 -e 2 -st 3 -p 0 -P 0
bash simulation/simulation.sh status
```

This runs 2 seeds × 3 strategy rows × 1 policy = 6 full-population simulations on 2 workers. A larger example is:

```bash
bash simulation/simulation.sh run -n 20 -s 1 -e 20 -st 271 -p 0 -P 4
```

This requests 27,100 simulations. `-n` sets concurrency; `-s` and `-e` specify the inclusive seed range; `-st` runs rows 1 through the specified strategy count; `-p` and `-P` specify the inclusive **zero-based** policy range. Choose worker counts to fit memory as well as CPU capacity, and do not exceed the number of scenarios. These command-line runs sample parameters as described above.

Workers are launched with `nohup`. Check `outputs/logs/node_<node>_nohup.log` for R output and errors, `node_<node>_master.log` for progress, and `outputs/txts/` for numerical summaries. PID files are stored in `outputs/pids/`. `status` reports process liveness, not verified successful simulation completion; inspect the R logs and expected result counts. Avoid overlapping launches in the same output directories because launcher files are reused and numerical logs append.

To run one strategy across seeds, use the simulation directory:

```bash
cd simulation
bash simulation.sh run_single -i 23 -c 4 -s 20 -p 1
```

Here `-i` is still a one-based strategy row, `-s` means the **number** of seeds (starting at 1), and `-p` is **one-based**: `1` means baseline and `5` means immediate ban. This subcommand converts the policy to zero-based internally. Its workers share node identifier 1 and therefore append to the same numerical log; use the `run` grid mode for separate per-worker numerical logs.

For a scheduler, use `run_hpc_node` with a different `-i` for each allocated worker and otherwise identical grid arguments:

```bash
# From the repository root; submit one such command per worker, IDs 1 through 20.
bash simulation/simulation.sh run_hpc_node -i 1 -n 20 -s 1 -e 20 -st 271 -p 0 -P 4
```

`simulation/submit.pbs` is a template to adapt to your cluster paths, R environment, and resources. The cluster also needs a platform-compatible engine library.

### Probabilistic sensitivity analysis (PSA)

For repeated no-screening projections, set `psa.flag` to `true` in `params/config.json`, then run:

```bash
cd simulation
mkdir -p ../outputs/txts ../outputs/lc_psa_output ../outputs/smoke_psa_output
bash psa.sh -n 100 -c 5 -p 0
bash psa.sh -s
```

The launcher runs `main.R 1 <seed> 1 <policy>` for seeds 1 through `-n`, always using strategy row 1. `-p` uses policy indices 0–4. The repetition count comes from `-n`, not `psa.samples`. For predictable allocation, choose a task count divisible by the worker count. Run policies sequentially with this launcher, since its status and log files share one location.

Progress and worker logs are under `outputs/lc_psa_output/`; numerical RDS outputs are in the two PSA directories described above. `-o` relocates the launcher's tracking/log directory, but does **not** change the R output paths in `main.R`. If stopping jobs manually, note that `psa.sh -k` includes a broad match for processes running `main.R`; it can affect other simulation batches.

### From new runs to analysis-ready results

[analysis/icer_table_construction.R](analysis/icer_table_construction.R) parses numerical text logs, matches each strategy to no screening within the same seed and tobacco policy, and calculates incremental outcomes. Its current input path (`../linux/txts_1_100`), strategy file (`strategy_add_MLS10.csv`), 33-row assumption, and output filename target a particular analysis. Adapt these to your run, including the strategy metadata alignment, before executing it from `analysis/`.

[analysis/prev_ind_mort_projection.R](analysis/prev_ind_mort_projection.R) combines PSA RDS files into annual cancer-burden summaries, five-year summaries, and smoking-prevalence matrices. Set its branch, policy, seed loop, and destination directories to match your runs. Its `Immediate_Ban` spelling differs from the runner's `Immediate_ban` filename on case-sensitive filesystems. Also, its annual `mean_death` column is currently calculated with `median()`; check the aggregation rather than interpreting that name as a mean.

These are analysis scripts with editable assumptions, not automatic steps performed by the launchers. The `aggressive`/`conservative` saved-result branches distinguish analyses with/without survival extrapolation; there is no JSON switch with those names in the current runner.

## Use the supplied simulation results

All paths referred to as `DEMOS_LC_result/outs_sim/` are under **`outputs/`** in this repository.

| Location within `outs_sim/` | Contents |
| --- | --- |
| `aggressive/` | Main screening table, with survival extrapolation, and policy-specific projections |
| `conservative/` | Screening table without survival extrapolation and corresponding projections |
| `<branch>/baseline/`, `mild/`, `moderate/`, `stringent/`, `immediate_ban/` | Annual cancer incidence/death RDS files, five-year summaries, and smoking-prevalence draws |
| `fhlc/` | Family-history screening analyses |
| `high_sensitivity/`, `low_sensitivity/` | Screening sensitivity alternatives |
| `packyear/` | Alternative pack-year eligibility analyses |
| `fig_s5.csv`, `fig_s5_obs.csv` | Fitted and observed cost data |

The main aggressive CSV contains 135,500 rows: 100 seeds × 271 strategies × 5 policies. `Seed`, `Strategy`, and `Smoking_ban_policy` identify a simulation scenario. Baseline Strategy 0 supplies the no-screening comparator within each seed and policy.

Key economic columns are `Cost`, `QALY`, `QALY_Gain`, `Additional_cost`, `ICER`, `LSA` (late-stage cases averted), `Deaths_Averted`, `OverDiagnosis`, `OD_rate`, and `FP_rate`. Costs are expressed in SGD in the figure recipes. In the construction script:

```text
QALY_Gain       = strategy QALY - baseline QALY
Additional_cost = strategy cost - baseline cost
ICER            = Additional_cost / QALY_Gain
LSA             = baseline late-stage cases - strategy late-stage cases
Deaths_Averted  = baseline deaths - strategy deaths
OverDiagnosis   = strategy diagnosed cases - baseline diagnosed cases
OD_rate         = 100 × OverDiagnosis / screen-detected cases
FP_rate         = 100 × false positives / screening rounds
```

Uncertainty summaries generally use the 2.5th and 97.5th percentiles across seeds. Keep the comparator and aggregation method explicit: a median of seed-specific ICERs is not generally the ratio of median incremental costs to median QALY gains. Baseline ratios can be undefined because the incremental denominator is zero.

## Derive figures with `code/functions.R`

[outputs/DEMOS_LC_result/code/functions.R](outputs/DEMOS_LC_result/code/functions.R) defines helpers; sourcing it alone does not produce figures. [plot.R](outputs/DEMOS_LC_result/plot.R) contains the figure-specific data preparation, labels, colours, layouts, and graphics-device calls.

### Minimal runnable figure from the saved projections

Start in R at the repository root. This example uses the actual `plot_lines()` and `plot_xy()` helpers to export baseline total lung cancer incidence with the stored uncertainty interval:

```r
library(dplyr)
library(grid)
source("outputs/DEMOS_LC_result/code/functions.R")

result_dir <- "outputs/DEMOS_LC_result"
inc <- readRDS(file.path(
  result_dir, "outs_sim/aggressive/baseline",
  "demos_lc_ind_death_with_ci_baseline.rds"
)) %>% filter(gender == -1) %>% arrange(year)

# Projection RDS gender codes: -1 = total, 0 = male, 1 = female.
plot_data <- list(as.matrix(inc[, c("mean_ind", "low_ind", "high_ind")]) / 1000)

# The grid plotting helpers read these layout variables from their environment.
margin <- c(3, 4, 1, 1)
xrange <- range(inc$year)
yrange <- c(0, max(plot_data[[1]][, 3]) * 1.05)
xtk <- xlab <- seq(2010, 2050, 10)
x_text <- "Year"
y_text <- "Lung cancer cases (thousands)"

dir.create("outputs/reproduced", recursive = TRUE, showWarnings = FALSE)
png("outputs/reproduced/baseline_incidence.png",
    width = 1600, height = 1000, res = 180)
grid.newpage()
plot_lines(plot_data, x_idx = inc$year, col = "#355C8A")
plot_xy()
dev.off()
```

To plot deaths, select `median_death`, `low_death`, and `high_death` and update the axis label and output filename. Policy comparisons read the corresponding files in each policy subdirectory.

### Prepare screening outcomes for the figure recipes

```r
library(dplyr)
library(parallel)
source("outputs/DEMOS_LC_result/code/functions.R")

proj <- read.csv(paste0(
  "outputs/DEMOS_LC_result/outs_sim/aggressive/",
  "demos_screening_result_with_smoking_ban_policy_table_with_survival_extrapolation.csv"
))

# proj_clean() expects this global metric vector, also defined by plot.R.
metrics <- c("QALY", "LSA", "Deaths_Averted", "Cost_Savings", "OD_rate", "FP_rate")
proj_by_seed <- proj_clean(proj, scale = FALSE, agg_by_seed = FALSE)
proj_summary <- proj_clean(proj, scale = FALSE, agg_by_seed = TRUE)
proj_scaled <- proj_clean(proj, scale = TRUE, agg_by_seed = TRUE)
```

`proj_clean()` removes the no-screening rows, derives common eligibility columns, and by default retains the 20-pack-year/15-year-quitting-period ever-smoker strategies. Use `filter_packyr = FALSE` for other pack-year thresholds and `ever_smoker = FALSE` for never-smoker eligibility analyses. It renames `QALY_Gain` to `QALY`, total `QALY` to `QALY_raw`, and `Additional_cost` to `Cost_Savings`. Despite its name, positive `Cost_Savings` therefore means **additional cost** in these prepared tables.

With `agg_by_seed = TRUE`, it returns medians and `_l`/`_u` percentile bounds. Scaling adds `_scaled` columns for comparisons and radar/heatmap displays; retain the unscaled data for reported numerical outcomes. Other helpers include `estim()` (mean or median and percentile bounds), `pareto_frontier()`, `compute_topsis_scores()`, `plot_rect()`, and `plot_radar()`. Helpers using `parallel::mclapply()` with multiple cores require a platform that supports forking, or adaptation to serial execution.

### Manuscript figure workflow

Open `plot.R` and work from `outputs/DEMOS_LC_result/` so its `outs/`, `outs_sim/`, and `code/` paths resolve. Load its libraries and helper functions, run the label definitions, select the relevant data-loading sections, and then run the preparation and plotting blocks for the desired figure. Outputs default to `plots/`; choose a separate `plot.dr` directory to retain existing figures.

The recipes cover smoking prevalence, stage-specific survival, fitted and projected cancer burden, screening heatmaps, radar plots, strategy selection, cost-effectiveness scatter plots, and sensitivity analyses. Supporting observed/fitted inputs come from `outs/` as well as `outs_sim/`.

The current file is an interactive analysis script and needs section selection before a complete run: its sensitivity loader refers to `outs_sim/screen_sensitivity/`, whereas this checkout stores those CSVs in `high_sensitivity/` and `low_sensitivity/`. Later family-history and pack-year setup blocks also overwrite `proj` and scenario variables. Select those inputs explicitly for the intended analysis; sourcing the whole file unchanged is not a one-command figure reproduction workflow.

## Derive tables and reported numbers with `analysis/derive.R`

[analysis/derive.R](analysis/derive.R) contains the numerical recipes. Its opening exploratory section needs the individual-level `populationdata` object from a simulation; aggregate CSVs cannot reproduce those individual-level summaries. The later table and manuscript-number sections work from saved result CSVs.

| Section | Result |
| --- | --- |
| Opening exploratory analysis | Cases by smoking status/sex/stage, smoking duration, onset age, and survival summaries |
| Table S6 | Absolute QALYs, late-stage cases, deaths, and costs, grouped by sex and tobacco policy |
| Table S7 | MLSOD10 strategies: male 22 and female 157 |
| Table S8 | TRS strategies: male 51 and female 186 |
| `Status quo with lung cancer screening` | Combined-sex screening outcomes relative to no screening |
| `Use MLSOD10 under status quo as baseline` | Changes in screening age, uptake, and interval |
| `Smoking ban with lung cancer screening` and discussion blocks | Comparisons between immediate ban and status quo |
| `FHLC` | Screening extended to people with family history of lung cancer |

In the supplied main table, MLSOD10 uses ages 50–80, annual screening, and 100% uptake; TRS uses ages 60–80, biennial screening, and 100% uptake. Both use the 20-pack-year and 15-year-quitting-period eligibility settings. The strategy IDs above are result-table IDs, not runner row arguments.

### Run the table sections directly

The script mixes root-relative paths in Tables S6–S8 with `../outputs/` paths in later sections, and each table block writes `temp.xlsx`. The following small reader runs the existing sections unchanged except for their export filenames. Start at the repository root in a fresh R session; it skips the exploratory section and preserves each table separately.

```r
library(dplyr)
library(tidyr)
library(writexl)

derive_code <- readLines("analysis/derive.R")
run_derive_section <- function(start_marker, end_marker, table_file = NULL) {
  first <- which(trimws(derive_code) == start_marker)
  last <- which(trimws(derive_code) == end_marker)
  stopifnot(length(first) == 1L, length(last) == 1L, last > first)
  code <- derive_code[first:(last - 1L)]
  if (!is.null(table_file)) {
    code <- sub('write_xlsx("temp.xlsx")',
                'write_xlsx(table_file)', code, fixed = TRUE)
  }
  # Keep output_icer and other intermediate objects between sections.
  env <- .GlobalEnv
  if (!is.null(table_file)) env$table_file <- table_file
  connection <- textConnection(code)
  on.exit(close(connection))
  source(connection, local = env, echo = FALSE, print.eval = TRUE)
}

dir.create("outputs/reproduced", recursive = TRUE, showWarnings = FALSE)
run_derive_section("# Table S6", "# Table S7", "outputs/reproduced/Table_S6.xlsx")
run_derive_section("# Table S7", "# Table S8", "outputs/reproduced/Table_S7.xlsx")
run_derive_section("# Table S8", "# Numbers in `Status quo with lung cancer screening`",
                   "outputs/reproduced/Table_S8.xlsx")
```

S7 and S8 reuse `output_icer` and the grouping fields prepared by S6, so run them in that order. S6 pools all nonbaseline strategies within each sex/policy group; its intervals describe that pooled set of strategies and seeds. S7 and S8 select the named strategies before summarising across seeds. The table code scales absolute QALYs to millions, case/death counts to thousands, and costs to billions; incremental tables scale QALY gains, cases/deaths averted, and ICERs to thousands and additional costs to billions. OD and FP remain percentages.

### Run the status-quo numerical comparisons

Continue in the same session using the section reader above:

```r
setwd("analysis")  # Later derive.R blocks read ../outputs/...
run_derive_section(
  "# Numbers in `Status quo with lung cancer screening`",
  "######## Use MLSOD10 under status quo as baseline ########"
)
setwd("..")
```

This prints the MLSOD10 and TRS combined-sex outcome summaries, including median seed-specific ICERs and 95% uncertainty intervals. Other blocks can be selected with their exact comment headings, or run interactively from `analysis/`. Run a block's reference-data preparation before its comparison calculations. The light-smoker block is commented out in the current file.

In this status-quo section, the displayed `Cost` row retains the male-strategy population total; the code combines sex-specific incremental costs in `AddCost` but does not update `Cost`. For total combined-program cost, add the combined `AddCost` to the matching no-screening population cost. With the supplied CSV, the median status-quo ICERs printed by this section are 44,226.18 SGD/QALY for MLSOD10 and 26,968.64 SGD/QALY for TRS.

Several comparisons in `derive.R` combine male and female rows using `cbind()`, and subtract paired vectors. These assume matching seed order and complete matching scenarios. Preserve the supplied ordering, or explicitly join by `Seed` and policy when adapting the analysis. For combined-sex programs, add the sex-specific **increments** to a single population baseline: adding two total population QALYs or costs would double-count the shared baseline. Recompute pooled OD/FP percentages from their summed counts rather than adding percentages.

## Package information

Package version: `0.0.1`. Maintainer: Yichen He (<e0732876@u.nus.edu>). The package declares GPL (>= 2) in `DemosMiscanLung/DESCRIPTION`.
