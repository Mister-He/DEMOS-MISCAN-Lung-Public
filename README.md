# DEMOS-MISCAN-Lung

DEMOS-MISCAN-Lung is an R/C++ discrete-event microsimulation of lung cancer
screening and tobacco-control policies in Singapore. It simulates smoking,
cancer natural history, screening, survival, costs, and QALYs at population
scale.

## Repository

| Path | Purpose |
| --- | --- |
| `DemosMiscanLung/` | R package and compiled simulation engine |
| `simulation/` | Single runs, batch runs, and PSA launchers |
| `params/`, `data/` | Configuration, model inputs, and fitted parameters |
| `modules/` | Smoking, cancer-risk, survival, and cost fitting |
| `analysis/` | Result aggregation and manuscript tables/numbers |
| `outputs/DEMOS_LC_result/` | Saved results, figure code, and plots |

## Installation

The reference environment is R 4.5.3 with a C++17 toolchain. Direct dependency
versions are listed in [requirements.txt](requirements.txt) and
[environment.yml](environment.yml). `requirements.txt` is a Conda match-spec
file, not a Python requirements file.

```bash
conda env create -f environment.yml
conda activate demos-miscan-lung
R CMD INSTALL --preclean DemosMiscanLung
```

The supplied `DemosMiscanLung/src/libmycode.a` contains macOS ARM64 objects;
new simulations on other platforms require a compatible engine build. Analysis
of saved results does not require package installation.

## Run simulations

Set options in [params/config.json](params/config.json). Defaults use seed 42,
strategy row 1 (no screening), baseline tobacco policy, year 2050, and age
truncation 90. Strategies are one-based rows in `params/strategy.csv`; row 1 is
the baseline. Policy indices are 0 = baseline, 1 = mild, 2 = moderate,
3 = stringent, and 4 = immediate ban.

For one full-population run, start a fresh R session at the repository root:

```r
dir.create("outputs/txts", recursive = TRUE, showWarnings = FALSE)
source("simulation/main.R")
print(result)
```

The initial population has approximately 2.74 million records. The script
appends summaries to `outputs/txts/log_node_<node>.txt`; save `populationdata`
explicitly if individual-level results are needed.

For command-line runs, the arguments are `node seed strategy_row policy_index`:

```bash
mkdir -p outputs/txts
Rscript simulation/main.R 1 42 1 0
```

Supplying arguments samples uncertain parameters, even when `psa.flag` is false.

For parallel runs:

```bash
# 2 seeds × 3 strategy rows × 1 policy = 6 runs
bash simulation/simulation.sh run -n 2 -s 1 -e 2 -st 3 -p 0 -P 0
bash simulation/simulation.sh status
```

The full grid is `run -n 20 -s 1 -e 20 -st 271 -p 0 -P 4` (27,100 runs);
choose concurrency for available memory. For repeated baseline PSA, set
`psa.flag = true` and run `cd simulation && bash psa.sh -n 100 -c 5 -p 0`.
Batch logs are under `outputs/logs/`; PSA RDS files are under
`outputs/lc_psa_output/` and `outputs/smoke_psa_output/`.

## Reproduce figures and tables

Saved inputs are in `outputs/DEMOS_LC_result/outs_sim/`. The `aggressive/` and
`conservative/` branches contain screening results with and without survival
extrapolation; `fhlc/`, `packyear/`, `high_sensitivity/`, and
`low_sensitivity/` contain additional analyses. No new simulation is needed.

### Figures

Use [plot.R](outputs/DEMOS_LC_result/plot.R), which uses
[code/functions.R](outputs/DEMOS_LC_result/code/functions.R). Run from
`outputs/DEMOS_LC_result/`, selecting the relevant data-preparation and plotting
sections. Outputs go to `plots/`.

| Output | `plot.R` section |
| --- | --- |
| `smoking_fit.png` | Smoking prevalence |
| `surv_smoke.png`, `surv_stage.png` | Stage-specific survival |
| `agg_inc.png` | Historical incidence and mortality |
| `yr_inc_*.png`, `fit+proj_*.png` | Annual and fitted cancer projections |
| `screen_heatmap_*.png` | Screening-strategy heatmaps |
| `select_*.png`, `select_all_*.png` | Strategy comparisons |
| `scatter_CEA_*.png`, `ratios_baseline_*.png` | Cost-effectiveness analyses |
| `FigureS1.png`, `FigureS2_turnado.png`, `cost_stage.png` | Figures S1–S3 sections |

`functions.R` provides `proj_clean()`, `estim()`, Pareto/TOPSIS helpers, and
plotting functions; sourcing it alone does not create figures. The current
sensitivity block refers to obsolete `screen_sensitivity/`; use
`high_sensitivity/` and `low_sensitivity/` instead.

### Tables and reported numbers

Use [analysis/derive.R](analysis/derive.R). Run `Table S6`, `Table S7`, and
`Table S8` in order, giving each `write_xlsx()` call a distinct output name.
Later sections derive reported numbers for status-quo screening, tobacco bans,
alternative screening settings, and family-history screening.

| Output | Section |
| --- | --- |
| Table S6 | Absolute QALYs, late-stage cases, deaths, and costs by sex/policy |
| Table S7 | MLSOD10 (Strategies 22 and 157) |
| Table S8 | TRS (Strategies 51 and 186) |
| Screening and policy results | `Status quo...`, `Smoking ban...`, and `FHLC` blocks |

The opening exploratory section requires the individual-level `populationdata`
object. Later sections use saved CSV results. They compare each strategy with
the same-seed, same-policy baseline and report median, 2.5th, and 97.5th
percentiles. Preserve seed ordering in paired comparisons.

## Data availability

Parameter files, available reference/calibration data, and saved simulation
outputs are included under `params/`, `data/`, `modules/`, and
`outputs/DEMOS_LC_result/`.

Some original survey/cohort, survival, and cost datasets referenced by training
scripts are not included. Their redistribution status and formal access process
are not documented here. For access questions, contact [Yichen He](mailto:e0732876@u.nus.edu)
with the dataset name, institution, intended use, and required variables; the
data custodian may require separate approval.

Package version: `DemosMiscanLung 0.0.1`. License: GPL (>= 2); see
[DemosMiscanLung/DESCRIPTION](DemosMiscanLung/DESCRIPTION).
