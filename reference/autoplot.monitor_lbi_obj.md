# Plot method for monitor_lbi() output

Plots the LBI CUSUM detector path against its boundary, one panel per
series, with a vertical marker at the alarm date.

## Usage

``` r
# S3 method for class 'monitor_lbi_obj'
autoplot(object, ...)
```

## Arguments

- object:

  An object of class `monitor_lbi_obj`, the output of
  [`monitor_lbi`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md).

- ...:

  Further arguments passed to methods. Not used.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)

## See also

[`monitor_lbi`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md)
