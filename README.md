# gtmiR

## Overview

An R package for analyzing and visualizing the latest data of the [GovTech Maturity Index (GTMI)](https://www.worldbank.org/en/programs/govtech/gtmi-2025-update). It provides functions to compute score changes, classify countries into performance tiers, and generate publication-ready plots.

Status: Work in Progress.

## Repository structure

```
├── raw-data/                          # Raw data and initial processing scripts
│   ├── 0.gtmi_data_cleaning.R         # GTMI panel data preparation
│   ├── 1.ims_adoption_data.R          # IMS adoption indicators preparation
│   └── output/                        # Intermediate outputs from cleaning scripts
├── analysis/                          # Analysis and plotting scripts
│   ├── source/                        # Numbered analysis scripts (run in order)
│   └── figs/                          # Generated figures
├── R/                                 # Exported package functions
│   ├── plot_funs.R                    # Core analysis and plotting functions
│   ├── data.R                         # Dataset documentation
│   └── globals.R                      # Global variable declarations
├── data/                              # Processed package datasets (lazy-loaded)
├── man/                               # Auto-generated R package documentation
├── renv/                              # Package dependency management (renv)
├── README.md                          # This file
├── gtmiR.Rproj                        # RStudio project file
├── DESCRIPTION                        # Package metadata and dependencies
├── NAMESPACE                          # Package namespace (auto-generated)
├── renv.lock                          # Locked dependency versions
├── .gitignore                         # Files ignored by Git
├── .Rbuildignore                      # Files excluded from package build
└── .Rprofile                          # Project-specific R startup settings
```

## Installation

``` r
# Step 1. Install the packages 'pak' or 'remotes' if you don't have them:

# install.packages("pak")
# install.packages("remotes")


# Step 2. Install cliaretl from GitHub:
remotes::install_github("WB-PIDA-Data-Science-Shop/gtmiR")
```

## Reproducing the analysis

Run the scripts in `analysis/source/` in order. Each script sources the package and expects the lazy-loaded datasets to be available.

``` r
# Restore dependencies first
renv::restore()

# Then run scripts in order
source("analysis/source/01-trends_overview.R")
source("analysis/source/02-regional-avgs.R")
source("analysis/source/03-sankey-group-letter.R")
source("analysis/source/04-corr-to-outcomes.R")
```

## Development

``` r
devtools::document()
devtools::check()
```
