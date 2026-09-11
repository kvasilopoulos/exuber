# Tidy a `radf_obj` object

Summarizes information about `radf_obj` object.

## Usage

``` r
# S3 method for class 'radf_obj'
tidy(x, format = c("wide", "long"), panel = FALSE, ...)
```

## Arguments

- x:

  An object of class `radf_obj`.

- format:

  Long or wide format (default = "wide").

- panel:

  If TRUE then returns the panel statistics

- ...:

  Further arguments passed to methods. Not used.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)

## Examples

``` r
# \donttest{
dta <- data.frame(psy1 = sim_psy1(n = 100), psy2 = sim_psy2(n = 100))

rfd <- radf(dta)

# Get the test statistic
tidy(rfd)
#> # A tibble: 2 × 4
#>   id      adf  sadf gsadf
#>   <fct> <dbl> <dbl> <dbl>
#> 1 psy1  -2.36  4.81  4.81
#> 2 psy2  -2.92  3.17  3.84

# Get the test statisticsequences
augment(rfd)
#> # A tibble: 162 × 6
#>      key index id     data  badf bsadf
#>    <int> <dbl> <chr> <dbl> <dbl> <dbl>
#>  1    20    20 psy1  115.  -1.29 -1.29
#>  2    20    20 psy2   98.2 -1.16 -1.16
#>  3    21    21 psy1  113.  -1.43 -1.43
#>  4    21    21 psy2  105.  -1.29 -1.29
#>  5    22    22 psy1  107.  -1.57 -1.57
#>  6    22    22 psy2  116.  -1.20 -1.20
#>  7    23    23 psy1   96.3 -1.52 -1.52
#>  8    23    23 psy2  115.  -1.26 -1.26
#>  9    24    24 psy1  101.  -1.69 -1.69
#> 10    24    24 psy2  123.  -1.00 -1.00
#> # ℹ 152 more rows

# Get the panel test statistic
tidy(rfd, panel = TRUE)
#> # A tibble: 1 × 1
#>   gsadf_panel
#>         <dbl>
#> 1        1.67
# }
```
