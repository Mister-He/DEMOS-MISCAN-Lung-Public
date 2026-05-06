DEMOS-MISCAN-Lung

A compact microsimulation for evaluating lung cancer screening strategies.

Prerequisites
- A C++17-compatible compiler (for Rcpp source files).
- R version 4.5.3 (or newer).
- The R package `Rcpp` installed (plus usual data packages: `dplyr`, `data.table`, `tidyr`, `jsonlite`, `ggplot2`).

Installation
1. From the project root, install the package (required before first run):

```bash
R CMD INSTALL -preclean DemosMiscanLung
```

2. Install recommended R packages in R:

```r
install.packages(c("dplyr", "data.table", "tidyr", "Rcpp", "jsonlite", "ggplot2"))
```

Quick Start
1. Ensure the package is installed (see Installation).
2. Configure `params/config.json` as needed.
3. Run a single simulation from the project root:

```r
setwd("simulation/")
source("main.R")
```

Batch runs and PSA
- Use `simulation/simulation.sh` for batch strategy runs.
- Use `simulation/psa.sh` for probabilistic sensitivity analysis.

Project layout (high level)
- `core/`: C++ simulation core and helpers
- `simulation/`: R scripts for running simulations and post-processing
- `modules/`: Training and calibration pipelines
- `params/`: Model parameters and strategy definitions
- `outputs/`: Simulation and analysis outputs
