# Plot method for radf_sbz_union() output

Plots the supDF, supBZ and union statistics of each series against their
bootstrap critical values at the chosen significance level.

## Usage

``` r
# S3 method for class 'radf_sbz_union_obj'
autoplot(object, sig_lvl = 95, ...)
```

## Arguments

- object:

  A `radf_sbz_union` object.

- sig_lvl:

  Significance level at which to plot the critical value, one of `90`,
  `95` (default) or `99`.

- ...:

  Further arguments passed to methods. Not used.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)

## See also

[`radf_sbz_union`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md)
