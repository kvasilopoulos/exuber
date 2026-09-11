# Tidy a `radf_cv` object

Summarizes information about `radf_cv` object.

## Usage

``` r
# S3 method for class 'radf_cv'
tidy(x, format = c("wide", "long"), ...)
```

## Arguments

- x:

  An object of class `radf_cv`.

- format:

  Long or wide format (default = "wide").

- ...:

  Further arguments passed to methods. Not used.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)

- id: The series names.

- sig: The significance level.

- name: The name of the series (when format is "long").

- crit: The critical value (when format is "long").

## Examples

``` r
# \donttest{
mc <- radf_mc_cv(100)

# Get the critical values
tidy(mc)
#> # A tibble: 3 × 4
#>   sig       adf  sadf gsadf
#>   <fct>   <dbl> <dbl> <dbl>
#> 1 90    -0.327  0.948  1.64
#> 2 95     0.0800 1.29   1.92
#> 3 99     0.742  1.84   2.49

# Get the critical value sequences
augment(mc)
#> # A tibble: 243 × 4
#>      key sig    badf   bsadf
#>    <int> <fct> <dbl>   <dbl>
#>  1    20 90    -0.44 -0.375 
#>  2    20 95    -0.08  0.0100
#>  3    20 99     0.6   0.668 
#>  4    21 90    -0.44 -0.226 
#>  5    21 95    -0.08  0.155 
#>  6    21 99     0.6   0.765 
#>  7    22 90    -0.44 -0.146 
#>  8    22 95    -0.08  0.244 
#>  9    22 99     0.6   0.820 
#> 10    23 90    -0.44 -0.0862
#> # ℹ 233 more rows
# }
```
