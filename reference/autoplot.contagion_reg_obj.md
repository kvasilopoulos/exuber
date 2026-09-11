# Plot method for contagion_reg() output

Plots the time-varying contagion coefficient \\\delta_2(r)\\ against the
sample fraction \\r\\, with a zero reference line.

## Usage

``` r
# S3 method for class 'contagion_reg_obj'
autoplot(object, ...)
```

## Arguments

- object:

  An object of class `contagion_reg_obj`, the output of
  [`contagion_reg`](https://kvasilopoulos.github.io/exuber/reference/contagion_reg.md).

- ...:

  Further arguments passed to methods. Not used.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)

## See also

[`contagion_reg`](https://kvasilopoulos.github.io/exuber/reference/contagion_reg.md)
