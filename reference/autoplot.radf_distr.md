# Plotting a `radf_distr` object

Takes a `radf_distr` object and returns a ggplot2 object.

## Usage

``` r
# S3 method for class 'radf_distr'
autoplot(object, ...)
```

## Arguments

- object:

  An object of class `radf_distr`.

- ...:

  Further arguments passed to methods, used only in `wb_distr` facet
  options.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)

## Examples

``` r
# \donttest{
# Monte Carlo distribution
mdist <- radf_mc_distr(n = 100, nrep = 1000)
autoplot(mdist)


# Wild bootstrap distribution (one facet per series)
wdist <- radf_wb_distr(sim_data)
autoplot(wdist)


# Panel sieve bootstrap distribution
sdist <- radf_sb_distr(sim_data, nboot = 500)
autoplot(sdist)

# }
```
