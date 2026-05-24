<!-- README.md is generated from README.Rmd. Please edit that file -->

# vecalc

<!-- badges: start -->

[![R-CMD-check](https://github.com/medewitt/vecalc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/medewitt/vecalc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

vecalc estimates vaccine efficacy from observational cohort data.
Vaccinated and unvaccinated groups are balanced on baseline covariates
using matching methods from
[MatchIt](https://kosukeimai.github.io/MatchIt/), and the matched event
counts are passed to a Bayesian binomial model fit with Stan (via
[cmdstanr](https://mc-stan.org/cmdstanr/)) to produce a posterior
distribution for vaccine efficacy (VE).

## Installation

Install the development version from
[R-universe](https://medewitt.r-universe.dev) or GitHub:

``` r
install.packages("vecalc", repos = "https://medewitt.r-universe.dev")

# or
# install.packages("remotes")
remotes::install_github("medewitt/vecalc")
```

vecalc fits models with CmdStan through cmdstanr. If you do not already
have them, install cmdstanr and CmdStan:

``` r
install.packages("cmdstanr", repos = "https://mc-stan.org/r-packages/")
cmdstanr::install_cmdstan()
```

## Example

A simulated cohort, `test_cohort`, ships with the package. The workflow
has two steps: prepare (match) the data, then fit the model.

``` r
library(vecalc)

data(test_cohort)

# 1. Balance vaccinated and unvaccinated groups on covariates
ve_data <- prepare_ve_data(
  vaccinated ~ age + race + sex,
  data = test_cohort,
  method = "cem"
)

# 2. Fit the Bayesian vaccine efficacy model
fit <- fit_ve(ve_data)

fit$sumz
```

## Code of Conduct

Please note that the vecalc project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
