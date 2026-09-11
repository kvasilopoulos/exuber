# Plot method for lbi_test() output

Bar chart of the LBI statistic per series against its critical value;
series that exceed it are flagged as detected.

## Usage

``` r
# S3 method for class 'lbi_test_obj'
autoplot(object, ...)
```

## Arguments

- object:

  An object of class `lbi_test_obj`, the output of
  [`lbi_test`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md).

- ...:

  Further arguments passed to methods. Not used.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)

## See also

[`lbi_test`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md)
