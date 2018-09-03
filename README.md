
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
  - `mc_cv()` : Monte Carlo Critical Values
  - `wb_cv()` : Wild Bootstrap Critical values

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
    Fortran](https://cran.r-project.org/bin/macosx/tools/)

If you encounter a clear bug, please file a reproducible example on
[GitHub](https://github.com/kvasilopoulos/exuber/issues).

## Usage

This is a basic example which shows you how to use exuber:

``` r
library(exuber)
# Simulate data witn n = 100 observations
set.seed(1234)
a1 <- sim_dgp1(n = 100) # one bubble
a2 <- sim_dgp2(n = 100) # two bubbles

dta <- data.frame("onebubble" = a1, 
                  "twobbubbles" = a2)

ts <- radf(dta, lag = 1)

# Monte Carlo critical values with parallel for faster computation
mc <- mc_cv(n = NROW(dta), parallel = T)
```

Report t-stats with the assiged critical values

``` r
report(ts, mc)
<<<<<<< HEAD
#> 
#>  Recursive Unit Root
#>  --------------------------------
#>  H0: Unit root
#>  H1: Explosive root
#>  --------------------------------
#>  Critical values: Monte Carlo 
#>  Minimum window: 19 
#>  Iterations: 2000 
#>  Lag: 1 
#>  --------------------------------
#>  onebubble 
#>        tstat     90%      95%    99%
#> ADF   -2.569 -0.3701 0.004608 0.6685
#> SADF   3.565  1.0158 1.384951 1.9083
#> GSADF  3.917  1.6840 1.975219 2.4931
#> 
#>  twobbubbles 
#>         tstat     90%      95%    99%
#> ADF   -2.5876 -0.3701 0.004608 0.6685
#> SADF   0.5412  1.0158 1.384951 1.9083
#> GSADF  2.0348  1.6840 1.975219 2.4931
=======
#> Warning: 'report' is deprecated.
#> Use 'summary' instead.
#> See help("Deprecated") and help("exuber-deprecated").
#> 
#>  Recursive Unit Root
#>  ------------------------------
#>  H0: Unit root
#>  H1: Explosive root
#>  ------------------------------
#>  Critical values: Monte Carlo 
#>  Minimum window: 19 
#>  Iterations: 2000 
#>  Lag: 
#>  ------------------------------
#>  onebubble 
#>        tstat     90%     95%   99%
#> ADF    -1.84  -0.432  -0.128  0.56
#> SADF    3.71   1.016   1.313  1.89
#> GSADF   4.02   1.694   2.003  2.47
#> 
#>  twobbubbles 
#>        tstat     90%     95%   99%
#> ADF    -2.68  -0.432  -0.128  0.56
#> SADF    4.99   1.016   1.313  1.89
#> GSADF   5.30   1.694   2.003  2.47
>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods
```

Date stamp periods of explosive behaviour

``` r
datestamp(ts, mc)
<<<<<<< HEAD
#> $onebubble
#>   Start End Duration
#> 1    27  28        2
#> 2    30  31        2
#> 3    45  55       11
#> 4    93 100        8
#> 
#> $twobbubbles
#>   Start End Duration
#> 1    31  40       10
#> 2    68  70        3
=======
#> 
#> Datestamp: Individual
#>  ------------------------------
#> onebubble :
#>   Start End Duration
#> 1    37  55       19
#> 
#> twobbubbles :
#>   Start End Duration
#> 1    26  40       15
#> 2    61  70       10
>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods
```

### Plotting

The output of plot is a list,

``` r
p1 <- plot(ts, mc, plot_type = "multiple", breaks_x = 20, breaks_y = 3)

# Use gridExtra to rearrange
library(gridExtra)
do.call("grid.arrange", c(p1, ncol = 2))
```

<img src="inst/figures/readme.png" width="900"/>

#### License

This package is free and open source software, licensed under
[GPL-3](https://github.com/kvasilopoulos/exuber/blob/master/LICENSE).

#### Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/kvasilopoulos/exuber/blob/master/CONDUCT.md).
By participating in this project you agree to abide by its terms.
