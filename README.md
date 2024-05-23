# Introduction

This GitHub repository contains the code used for the draft "Reservoir Computing in R: a Tutorial for Using reservoirnet to Predict Complex Time-Series". The repository is organized as follows:

## `jss_reproducibility.R`

This R script assesses the reproducibility of the code. It utilizes `knitr::purl("draft_jss/draft_jss.Rmd")` for full reproducibility. To run this script, the following packages are required:
- dplyr
- ggplot2
- patchwork
- reservoirnet

Additionally, the file `data/precomputed_results.rds` is needed for the last part of the script, containing precomputed results from a computation cluster server.

## `data`

### `df_obfuscated_epidemio.rds`

This file contains the data used for the last use case of the paper.

### `precomputed_results.rds`

Precomputed results from a computation cluster server for the last use case of the paper.

## `functions`

This folder contains several functions used for the last use case of the paper, including data preparation and model fitting.

## `script`

This folder contains all scripts used on the computation server.

### `find_hyperparameter`

Scripts used to find relevant hyperparameters for both elastic-net and reservoir computing (echo state network abbreviated as ESN).

### `forecast`

Scripts used to forecast with elastic-net and reservoir computing after hyperparameters are set. The file `forecast_test_eval.R` gathers all the results, aggregates them, and prepares the plot used in the article.
