# Augment a `radf_cv` object

Returns the full critical-value sequences (badf_cv, bsadf_cv) of a
`radf_cv`, one row per observation; see
[`tidy.radf_cv`](https://kvasilopoulos.github.io/exuber/reference/tidy.radf_cv.md)
for the scalar critical values.

## Usage

``` r
# S3 method for class 'radf_cv'
augment(x, format = c("wide", "long"), trunc = TRUE, ...)
```

## Arguments

- x:

  An object of class `radf_cv`.

- format:

  Long or wide format (default = "wide").

- trunc:

  Whether to remove the period of the minimum window from the plot
  (default = TRUE).

- ...:

  Further arguments passed to methods. Not used.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
