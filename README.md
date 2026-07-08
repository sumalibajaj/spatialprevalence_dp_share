# Spatial clustering of SARS-CoV-2 prevalence in England using Dirichlet processes

This repository contains the R code accompanying the paper:

> **Evolution of the spatial scales of transmission of SARS-CoV-2 in England**  
> Sumali Bajaj, Ioana Bouros, Katherine M. Shepherd, Anne Cori, Moritz U.G. Kraemer, Richard Creswell\*, Ben Lambert\*  *(in preparation)*
> \*Equal contribution

## Overview

We apply a Bayesian nonparametric framework — specifically Dirichlet Process Mixture Models (DPMMs) — to cluster lower-tier local authorities (LTLAs) in England by their weekly SARS-CoV-2 prevalence trends, without pre-specifying the number of clusters. Clustering is performed independently for each month from October 2020 to February 2022. We then use linear regression to quantify how mobility, spatial proximity, and demographic factors predict which pairs of LTLAs co-cluster.

## Repository structure

```
spatialprevalence_dp_share/
├── data/
│   ├── raw/                   # Input data (prevalence, covariates, shapefiles)
│   └── processed/             # MCMC outputs and derived datasets
├── outputs/                   # Figures (maps, trends, PPC, epidemiological analysis)
└── src/R/
    ├── functions/             # Core functions (DP Gibbs sampler, plotting, etc.)
    ├── run_clustertrends.R    # Main script: run clustering for all months
    ├── create_clustertrends.R # Creates cluster assignments via MCMC
    ├── create_select_processed_data.R  # Processes raw covariate data
    ├── do_epianalysis.R       # Linear regression analysis of co-clustering drivers
    ├── run_and_plot_epianalysis.R
    ├── plot_all_data.R        # National prevalence + mobility + covariate plots
    ├── plot_cluster_characteristics.R
    ├── plot_maps_trends.R     # Monthly cluster maps and trend plots
    ├── plot_prev_ppc.R        # Posterior predictive checks
    ├── run_plot_R2.R
    ├── sensitivity_alpha.R    # Sensitivity to concentration parameter α
    ├── sensitivity_sigma.R    # Sensitivity to likelihood covariance σ
    └── sensitivity_all_chains.R
```

## Data

**Prevalence estimates** (`data/raw/ltla_in_region_debiased_prev.csv`): Weekly LTLA-specific SARS-CoV-2 prevalence for England.

**Mobility** (`data/processed/covariates/ltla_monthly_mobility_processed_between.csv` and `data/processed/covariates/ltla_monthly_mobility_processed_within.csv`): (synthetic) Aggregated monthly mobility within and between LTLAs. The mobility dataset included in this repository is synthetic and provided for reproducibility purposes. Real mobility data are not publicly available due to licensing restrictions.

**Covariates**: Index of Multiple Deprivation (IMD 2019), population density, age structure (proportion over 64), and LTLA boundary shapefiles — all from ONS and included in `data/raw/`.

## Running the analysis

All scripts should be run from the project root (open `spatialprevalence_dp_share.Rproj` in RStudio).

### 1. Process covariates
```r
source("src/R/create_select_processed_data.R")
```

### 2. Run clustering (all months)
```r
source("src/R/run_clustertrends.R")
```
This runs the Gibbs sampler with 4 chains × 10000 iterations per month, saves MCMC outputs to `data/processed/`, and saves cluster map and trend plots to `outputs/`.

### 3. Posterior predictive checks
```r
source("src/R/plot_prev_ppc.R")
```

### 4. Epidemiological analysis (co-clustering regression)
```r
source("src/R/run_and_plot_epianalysis.R")
```

### 5. Sensitivity analyses
```r
src/R/sensitivity_alpha.R"   # varying α ∈ {1, 2, 5}
src/R/sensitivity_sigma.R   # varying σ ∈ {1e-4, 1e-5, 1e-6}
src/R/sensitivity_all_chains.R   # number of clusters across MCMC chains for all months
```

## Key model settings

| Parameter | Value | Description |
|-----------|-------|-------------|
| `alpha` | 2 | DP concentration parameter (prior expectation ≈ 10 clusters) |
| `sigma_mult_factor` | 1/100000 (i.e. σ = 1×10⁻⁵) | Diagonal of likelihood covariance matrix |
| `maxIters` | 2000 | MCMC iterations per chain |
| Chains | 4 | Independent chains with diverse initialisations |
| Burn-in | 50% | First half of iterations discarded |

## Dependencies

R packages: `tidyverse`, `lubridate`, `sf`, `ggplot2`, `reshape2`, `MASS`. Install with:

```r
install.packages(c("tidyverse", "lubridate", "sf", "ggplot2", "reshape2", "MASS"))
```

## Citation

If you use this code, please cite:

> Bajaj S, Bouros I, Shepherd KM, Cori A, Kraemer MUG, Creswell R\*, Lambert B\*. Evolution of the spatial scales of transmission of SARS-CoV-2 in England. *(in preparation)*

The Gibbs sampling implementation is adapted from [Li (2019)](https://www.sciencedirect.com/science/article/abs/pii/S0022249618301068).

## License

See [LICENSE](LICENSE).
