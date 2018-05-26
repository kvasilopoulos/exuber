
<!-- README.md is generated from README.Rmd. Please edit that file -->

# exdyn : Econometric Analysis of Explosive Time Series

[![Build
Status](https://travis-ci.org/kvasilopoulos/exdyn.svg?branch=master)](https://travis-ci.org/kvasilopoulos/exdyn)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/kvasilopoulos/exdyn?branch=master&svg=true)](https://ci.appveyor.com/project/kvasilopoulos/exdyn)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/exdyn)](https://cran.r-project.org/package=exdyn)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![codecov](https://codecov.io/gh/kvasilopoulos/exdyn/branch/master/graph/badge.svg)](https://codecov.io/gh/kvasilopoulos/exdyn)

## Description

Testing for and dating periods of explosive dynamics (exuberance) in
time series. Simulating periodically-collapsing speculative bubbles.

## Overview

  - `radf()`
  - `mc_cv()`
  - `wb_cv()`

## Installation

The package is still under development, to install the development
version from GitHub:

``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kvasilopoulos/exdyn")
```

Note that development version requires compilation, so to install you
will need the appropriate development tools.

  - Window Users should install
    [Rtools](https://cran.r-project.org/bin/windows/Rtools/)
  - Mac User should install [Clang or GNU
    Fortran](https://cran.r-project.org/bin/macosx/tools/)

If you encounter a clear bug, please file a reproducible example on
[GitHub](https://github.com/kvasilopoulos/exdyn/issues).

## Usage

This is a basic example which shows you how use exdyn:

``` r
library(exdyn)
set.seed(123)
a1 <- sim_dgp1(200) # one bubble
a2 <- sim_dgp2(200) # two bubbles
a3 <- sim_blan(200) # blanchard model
a4 <- sim_evans(200) # evans model


dfrm <- data.frame(a1, a2, a3, a4)
colnames(dfrm) <- c("oneb", "twob", "blan", "evans")

ts <- radf(dfrm, lag = 1)

# Critical Values mc = Monte Carlo, wb= Wild Bootstrapped
## Use 500 repetions(boostraps) for faster computation
mc <- mc_cv(NROW(dfrm), nrep = 500, parallel = T)
wb <- wb_cv(dfrm, nboot = 500, parallel = T)
```

### Report

``` r
report(ts, mc)
diagnostics(ts, mc)
datestamp(ts, mc)
```

### Plotting

The output of plot will be a list,

``` r
# All together
plot(ts, mc, plot_type = "single", breaks_x = 20)

# Individually
plot(ts, mc, plot_type = "multiple", breaks_x = 20)

library(gridExtra)
p1 <- plot(ts, mc, plot_type = "multiple", breaks_x = 20, breaks_y = 3)
do.call("grid.arrange", c(p1, ncol = 2))
```

-----

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/kvasilopoulos/exdyn/blob/master/CONDUCT.md).
By participating in this project you agree to abide by its terms.
