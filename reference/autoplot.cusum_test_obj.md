# Plot method for cusum_test() output

Plots the CUSUM-type statistic path against its critical value(s), one
panel per series; the two-sided CUSUM-of-squares tests show the sup path
against the upper and the inf path against the lower critical value.

## Usage

``` r
# S3 method for class 'cusum_test_obj'
autoplot(object, ...)
```

## Arguments

- object:

  An object of class `cusum_test_obj`, the output of
  [`cusum_test`](https://kvasilopoulos.github.io/exuber/reference/cusum_test.md).

- ...:

  Further arguments passed to methods. Not used.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)

## See also

[`cusum_test`](https://kvasilopoulos.github.io/exuber/reference/cusum_test.md)
