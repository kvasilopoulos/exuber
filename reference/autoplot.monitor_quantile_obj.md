# Plot method for monitor_quantile() output

Plots the quantile monitoring statistic against its boundary, one panel
per series, with a vertical marker at the alarm date.

## Usage

``` r
# S3 method for class 'monitor_quantile_obj'
autoplot(object, ...)
```

## Arguments

- object:

  An object of class `monitor_quantile_obj`, the output of
  [`monitor_quantile`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md).

- ...:

  Further arguments passed to methods. Not used.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)

## See also

[`monitor_quantile`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md)
