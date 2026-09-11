# Plot method for quantile_test() output

Bar chart of the quantile-DF statistic per series against its critical
value; series that exceed it are flagged as detected.

## Usage

``` r
# S3 method for class 'quantile_test_obj'
autoplot(object, ...)
```

## Arguments

- object:

  An object of class `quantile_test_obj`, the output of
  [`quantile_test`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md).

- ...:

  Further arguments passed to methods. Not used.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)

## See also

[`quantile_test`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)
