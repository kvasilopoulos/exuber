# Plot method for monitor_cusum() output

Plots the CUSUM detector path against its boundary, one panel per
series, with vertical markers at the end of the training sample and at
the alarm date.

## Usage

``` r
# S3 method for class 'monitor_cusum_obj'
autoplot(object, ...)
```

## Arguments

- object:

  An object of class `monitor_cusum_obj`, the output of
  [`monitor_cusum`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md).

- ...:

  Further arguments passed to methods. Not used.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)

## See also

[`monitor_cusum`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md)
