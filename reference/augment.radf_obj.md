# Augment a `radf_obj` object

Returns the full test-statistic sequences (badf, bsadf) of a `radf_obj`,
one row per observation; see
[`tidy.radf_obj`](https://kvasilopoulos.github.io/exuber/reference/tidy.radf_obj.md)
for the scalar statistics.

## Usage

``` r
# S3 method for class 'radf_obj'
augment(x, format = c("wide", "long"), panel = FALSE, trunc = TRUE, ...)
```

## Arguments

- x:

  An object of class `radf_obj`.

- format:

  Long or wide format (default = "wide").

- panel:

  If TRUE then returns the panel statistics

- trunc:

  Whether to remove the period of the minimum window from the plot
  (default = TRUE).

- ...:

  Further arguments passed to methods. Not used.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
