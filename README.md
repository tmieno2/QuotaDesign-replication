# Groundwater quota design: code and data

Code and data for the paper "Groundwater Management through Quotas: Economic and Water-Use Implications of Design Choices" by Taro Mieno, Timothy Foster, Nicholas Brozovic, and Renata Rimsaite.

The paper models a farmer who chooses an irrigation strategy each year under a groundwater quota and uncertain weather. It solves the farmer's dynamic optimization problem for 2,088 quota designs, which differ in the annual quota, the allocation period length, the maximum carry-over ratio, and the penalty multiplier, and then simulates farmers who follow the optimal decision rule.

## Contents

| Path | What it is |
|------|------------|
| `code/01-data-prep.qmd` | Step 1. Reads the crop model output, keeps the irrigation strategies on the yield-irrigation frontier, and sets prices and the discount rate. Fast. |
| `code/02-1-solve-DP.qmd` | Step 2. Solves the farmer's problem by backward induction for each quota design. Slow; writes about 7.5 GB to `results/optimal_choice/`. |
| `code/03-1-simulate.qmd` | Step 3. Simulates 500,000 farmers per design over 50 years (54 for three-year allocation periods), plus an endowment run and a 10,000-farmer run for the distribution figure. Slow. |
| `code/05-make-figures-tables.qmd` | Step 5. Makes all figures and Tables 1 and 2. A few minutes. |
| `code/R/` | Functions for backward induction (`backward_induction.R`) and simulation (`simulate.R`). |
| `code/00-simulation-overview.qmd` | A walkthrough of the method and how the steps fit together. Not needed to run anything. |
| `data/AquaCropSimulations_October2017/ProductionFunction_October2017.mat` | The only raw input: AquaCrop-OS simulations of irrigated corn in Chase County, Nebraska, giving yield and irrigation for each of 27,225 irrigation strategies in each of 35 historical weather years, plus the four soil moisture targets that define each strategy. |
| `results/simulation/sim_results_all.rds`, `results/sim_results_with_end.rds`, `results/sim_dist.rds` | Outputs of step 3, included so that step 5 can be run without steps 2 and 3. |
| `run_all.R` | Runs the steps in order. |

There is no step 4; the numbering follows the project's internal file names.

## Requirements

R (tested with R 4.5.0) and these packages:

```r
install.packages(c(
  "data.table", "dplyr", "flextable", "ggplot2", "ggridges", "ggthemes",
  "here", "knitr", "magrittr", "mgcv", "officer", "pbmcapply", "R.matlab",
  "scam", "viridis", "viridisLite"
))
```

Steps 2 and 3 run designs in parallel with `parallel::mclapply`, which uses multiple cores on macOS and Linux only.

## How to run

Run from the repository root, so that `.Rprofile` loads the packages and the functions in `code/R/` (for example, open `QuotaDesign.Rproj` in RStudio, or `cd` into the folder before calling `Rscript`).

```sh
Rscript run_all.R 05     # figures and tables from the included outputs
Rscript run_all.R        # everything: steps 1, 2, 3 and 5
```

Figures are written to `writing/figures/` and Tables 1 and 2 to `results/qd_table_NE.rds` and `results/qd_table_KS.rds`.

## Where each paper item comes from

| Paper item | File in `writing/figures/` | Step |
|------------|----------------------------|------|
| Figure 1 | `path_AQ_10_pen_2_co_varied.png` | 5 |
| Figure 2 | `path_AQ_10_co_1_pen_varied.png` | 5 |
| Figure 3 | `g_value_endowment.png` | 5 |
| Figure 4 | `g_ef_frontier_co.png` | 5 |
| Figure 5 | `g_variability.png` | 5 |
| Tables 1 and 2 | `qd_table_NE.png`, `qd_table_KS.png` | 5 |
| SI Figure S1 | `g_ef_frontier.png` | 1 |
| SI Figure S2 | `g_ex_end.png` | 5 |
| SI Figures S3 and S4 | `path_AQ_8_pen_2_co_varied.png`, `path_AQ_12_pen_2_co_varied.png` | 5 |
| SI Figure S5 | `g_cumulative_paths.png` | 5 |

Files ending in `_presentation.png` are larger-font versions for slides.

## Notes

- Random draws use fixed seeds, so re-running step 3 reproduces the included outputs exactly.
- Weather each year is drawn independently, with equal probability, from the 35 historical weather years.
- Dollar totals over a horizon are undiscounted sums of mean annual profit.
