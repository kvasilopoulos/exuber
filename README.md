
<!-- README.md is generated from README.Rmd. Please edit that file -->

# exuber : Econometric Analysis of Explosive Time Series

[![Build
Status](https://travis-ci.org/kvasilopoulos/exuber.svg?branch=master)](https://travis-ci.org/kvasilopoulos/exuber)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/kvasilopoulos/exuber?branch=master&svg=true)](https://ci.appveyor.com/project/kvasilopoulos/exuber)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/exuber)](https://cran.r-project.org/package=exuber)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![codecov](https://codecov.io/gh/kvasilopoulos/exuber/branch/master/graph/badge.svg)](https://codecov.io/gh/kvasilopoulos/exuber)

## Description

Testing for and dating periods of explosive dynamics (exuberance) in
time series using recursive unit root tests as proposed by [Phillips, P.
C., Shi, S. and Yu, J. (2015a)](https://doi.org/10.1111/iere.12132).
Simulate a variety of periodically-collapsing bubble models. The
estimation and simulation utilize the matrix inversion lemma from the
recursive least squares algorithm, which results in a significant speed
improvement.

## Overview

### Estimation

  - `radf()` : Recursive Augmented Dickey-Fuller test

### Simulate custom critical values

  - `mc_cv()` : Monte Carlo critical values
  - `wb_cv()` : Wild Bootstrap critical values
  - `sb_cv()` : Sieve Bootstrap (panel) critical values

### Simulation

  - `sim_dgp1()` : Simulation of a single-bubble process
  - `sim_dgp2()` : Simulation of a two-bubble process
  - `sim_blan()` : Simulation of a Blanchard (1979) bubble process
  - `sim_evans()` : Simulation of a Evans (1991) bubble process
  - `sim_div()` : Simulation of dividends

## Installation

You can install the released version of exuber from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("exuber")
```

And the development version from [GitHub](https://github.com/) with:

``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kvasilopoulos/exuber")
```

Note that development version requires compilation, so to install you
will need the appropriate development tools.

  - Window Users should install
    [Rtools](https://cran.r-project.org/bin/windows/Rtools/)
  - Mac User should install [Clang or GNU
    Fortran](https://cran.r-project.org/bin/macosx/tools/).

If you encounter a clear bug, please file a reproducible example on
[GitHub](https://github.com/kvasilopoulos/exuber/issues).

## Usage

This is a basic example which shows you how to use exuber:

``` r
library(exuber)
# Simulate data witn n = 100 observations
set.seed(112)
sim1 <- sim_dgp1(n = 100) # one bubble
sim2 <- sim_dgp2(n = 100) # two bubbles

dta <- data.frame("onebubble" = sim1, 
                  "twobbubbles" = sim2)

ts <- radf(dta, lag = 1)
```

Report t-stats with the assigned critical values

``` r
summary(ts)
#> 
#>  Recursive Unit Root
#>  ----------------------------------
#>  H0: Unit root
#>  H1: Explosive root
#>  ----------------------------------
#>  Critical values: Monte Carlo 
#>  Minimum window: 19 
#>  Iterations: 2000 
#>  Lag: 1 
#>  ----------------------------------
#>  onebubble 
#>         tstat      90%      95%     99%
#> ADF    -2.304  -0.4035  -0.0762  0.5104
#> SADF    4.522   1.0074   1.3484  1.9454
#> GSADF   4.718   1.6942   1.9259  2.6191
#> 
#>  twobbubbles 
#>         tstat      90%      95%     99%
#> ADF    -2.411  -0.4035  -0.0762  0.5104
#> SADF    4.057   1.0074   1.3484  1.9454
#> GSADF   5.571   1.6942   1.9259  2.6191
```

Date stamp periods of explosive behaviour

``` r
datestamp(ts)
#> 
#> Datestamp: Individual
#>  -----------------------------------
#> onebubble :
#>   Start End Duration
#> 1    40  55       16
#> 
#> twobbubbles :
#>   Start End Duration
#> 1    24  40       17
#> 2    62  70        9
```

### Plotting

The output of autoplot is a list of ggplots

``` r
ts %>% 
  autoplot() %>% 
  ggarrange()
```

<img src="inst/figures/readme.png" width="900"/>

#### License

This package is free and open source software, licensed under
[GPL-3](https://github.com/kvasilopoulos/exuber/blob/master/LICENSE).

#### Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/kvasilopoulos/exuber/blob/master/CONDUCT.md).
By participating in this project you agree to abide by its terms.
