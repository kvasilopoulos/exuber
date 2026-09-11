# Plot method for monitor() output

Plots the monitoring statistic against its boundary, one panel per
series, with vertical markers at the end of the training sample and at
the alarm date.

## Usage

``` r
# S3 method for class 'monitor_obj'
autoplot(object, ...)
```

## Arguments

- object:

  An object of class `monitor_obj`, the output of
  [`monitor`](https://kvasilopoulos.github.io/exuber/reference/monitor.md).

- ...:

  Further arguments passed to methods. Not used.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)

## See also

[`monitor`](https://kvasilopoulos.github.io/exuber/reference/monitor.md)
