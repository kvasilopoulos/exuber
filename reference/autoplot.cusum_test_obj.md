# Plot method for cusum_test() output

Plots the CUSUM-type statistic path against its critical values, with
one panel for each series. For the two-sided CUSUM-of-squares tests, the
sup path is shown against the upper critical value and the inf path
against the lower one.

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
