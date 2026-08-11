# Plotting a `ds_radf` object

Takes a `ds_radf` object and returns a ggplot2 object, with a
[geom_segment](https://ggplot2.tidyverse.org/reference/geom_segment.html)
layer.

## Usage

``` r
# S3 method for class 'ds_radf'
autoplot(object, trunc = TRUE, ...)
```

## Arguments

- object:

  An object of class `ds_radf`. The output of
  [`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)

- trunc:

  Whether to remove the period of the minimum window from the plot
  (default = TRUE).

- ...:

  Further arguments passed to methods. Not used.

## Value

[ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)

## Examples

``` r
# \donttest{

sim_data_wdate %>%
  radf() %>%
  datestamp() %>%
  autoplot()
#> Using `date` as index variable.
#> Using `radf_crit` for `cv`.
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the exuber package.
#>   Please report the issue at <https://github.com/kvasilopoulos/exuber/issues>.


# Change the colour manually
sim_data_wdate %>%
  radf() %>%
  datestamp() %>%
  autoplot() +
  ggplot2::scale_colour_manual(values = rep("black", 4))
#> Using `date` as index variable.
#> Using `radf_crit` for `cv`.

# }
```
