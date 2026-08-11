# Tidy a `radf_distr` object

Summarizes information about `radf_distr` object.

## Usage

``` r
# S3 method for class 'radf_distr'
tidy(x, ...)
```

## Arguments

- x:

  An object of class `radf_distr`.

- ...:

  Further arguments passed to methods. Not used.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)

## Examples

``` r
if (FALSE) { # \dontrun{
mc <- mc_cv(n = 100)

tidy(mc)
} # }
```
