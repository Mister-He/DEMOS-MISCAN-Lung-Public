# Changelog

## 1.1.0 — 2026-11-18

- Add rebuttal sensitivity analyses `SA_TOPSIS_weights.R` (TOPSIS criterion weights) and `SA_subset_runs.R` (strategy selection on 50- and 75-run subsets), with results in `outputs/DEMOS_LC_result/outs/`.
- Add raw HPC simulation results in `outputs/linux/`, processed into `outputs/DEMOS_LC_result/outs_sim/` by `analysis/icer_table_construction.R`.
- Add 2025–2050 screening-eligibility proportions for status-quo and immediate-ban scenarios, and update the PSA output.
- Rewrite `analysis/derive.R` to reproduce the numbers reported in the manuscript and SI.
- Update the README for the new outputs, analyses and `derive.R`.
- Set the `DemosMiscanLung` package version to 1.1.0 to match the release.
- Add GitHub issue templates for bug reports and feature requests.

## 1.0.1 / 1.0.0 — 2026-09-21

Both tags point to the same commit.

- Add a Conda environment and matching dependency list for the R and C++ simulation workflow.
- Expand the README with installation, simulation, and saved-result analysis instructions.
- Add `analysis/derive.R` to reproduce manuscript tables and reported results.
- Include the R package, simulation inputs, and saved outputs for the DEMOS-MISCAN-Lung model.
