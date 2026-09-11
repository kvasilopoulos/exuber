# Plot method for rootstamp() output on a single sub-sample

Plots the sub-sample against the fitted explosive path implied by the
estimated root, \\y_1 \rho^{t-1}\\.

## Usage

``` r
# S3 method for class 'rootstamp_est'
autoplot(object, ...)
```

## Arguments

- object:

  An object of class `rootstamp_est`, the output of the default
  [`rootstamp`](https://kvasilopoulos.github.io/exuber/reference/rootstamp.md)
  method.

- ...:

  Further arguments passed to methods. Not used.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)

## See also

[`rootstamp`](https://kvasilopoulos.github.io/exuber/reference/rootstamp.md)
