# Plot method for rootstamp() output on datestamped episodes

Plots the estimated root and its confidence interval for every episode,
one panel per series.

## Usage

``` r
# S3 method for class 'rootstamp_episodes'
autoplot(object, ...)
```

## Arguments

- object:

  An object of class `rootstamp_episodes`, the output of the `radf_obj`
  [`rootstamp`](https://kvasilopoulos.github.io/exuber/reference/rootstamp.md)
  method.

- ...:

  Further arguments passed to methods. Not used.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)

## See also

[`rootstamp`](https://kvasilopoulos.github.io/exuber/reference/rootstamp.md)
