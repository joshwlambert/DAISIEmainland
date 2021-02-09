
# DAISIEmainland

<!-- badges: start -->
[![R build status](https://github.com/joshwlambert/DAISIEmainland/workflows/R-CMD-check/badge.svg)](https://github.com/joshwlambert/DAISIEmainland/actions)
<!-- badges: end -->

The goal of DAISIEmainland is to ...

## Installation

You can install the released version of DAISIEmainland from [CRAN](https://CRAN.R-project.org) with:

``` r
remotes::install_github("joshwlambert/DAISIEmainland")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(DAISIEmainland)

island <- sim_island_mainland(
  time = 5,
  m = 100,
  island_pars = c(1, 1, 10, 0.1, 1),
  mainland_ext = 1,
  mainland_sample_prob = 1,
  replicates = 1,
  verbose = FALSE
)
```

