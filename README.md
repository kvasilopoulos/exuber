<!-- README.md is generated from README.Rmd. Please edit that file -->
exdyn
=====

The goal of exdyn is to ...

Installation
------------

The package is still under development, to install the development verison from GitHub:

``` r
install.packages("devtools")
devtools::install_github("kvasilopoulos/exdyn")
```

If you encounter a clear bug, please file a reproducible example on [GitHub](https://github.com/kvasilopoulos/exdyn/issues).

Example
-------

This is a basic example which shows you how use exdyn:

``` r
library(exdyn)

## Simulate a random walk with 300 observations
library(quantmod)
getSymbols("AAPL")
a1 <- AAPL[,"AAPL.Close"]
set.seed(199)
a2 <- sim_ar(200)
set.seed(199)
a3 <- sim_rw(200)
data <- data.frame(a1[1:200], a2, a3)
colnames(data) <- c("AAPL","ar1","rw")

# Visualize the data
plot.ts(data)

# Compute the test-statistics
tstats <- radf(data)

# Simulate the critical values using Monte-Carlo
# We use little repetitions for reproducible easing
mc_critical <- mc_cv(NROW(data), 400, parallel = T)
# wb_critical <- wb_cv(dfrm, 40, parallel = T) 
```

Report
------

``` r
summary(tstats, mc_critical)
diagnostics(tstats, mc_critical)
datestamp(tstats, mc_critical)
```

Plotting
--------

The output of plot will be a list

``` r
p <- plot(tstats, mc_critical, break_x = "2 weeks", format = "%m-%Y")

library(gridExtra)
library(grid)

do.call(grid.arrange, p)

# Add ggtitles
titlenames <- diagnostics(tsats, mc_critical, echo = FALSE)
for (i in seq_along(d)) pnames[[i]] <- p[[i]]+ggtitle(titlenames[i])
do.call(grid.arrange, pnames)
```
