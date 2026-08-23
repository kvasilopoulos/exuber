# Tidy a `radf_obj` object

Summarizes information about `radf_obj` object.

## Usage

``` r
# S3 method for class 'radf_obj'
tidy(x, format = c("wide", "long"), panel = FALSE, ...)

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

- ...:

  Further arguments passed to methods. Not used.

- trunc:

  Whether to remove the period of the minimum window from the plot
  (default = TRUE).

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
#>   id      adf   sadf  gsadf
#>   <fct> <dbl>  <dbl>  <dbl>
#> 1 psy1  -1.94 -0.685 -0.223
#> 2 psy2  -2.52  4.65   5.36 

# Get the test statisticsequences
augment(rfd)
#> # A tibble: 162 × 6
#>      key index id     data   badf  bsadf
#>    <int> <dbl> <chr> <dbl>  <dbl>  <dbl>
#>  1    20    20 psy1  111.  -1.62  -1.62 
#>  2    20    20 psy2   83.0 -0.993 -0.993
#>  3    21    21 psy1   99.1 -2.19  -2.15 
#>  4    21    21 psy2   82.3 -1.02  -1.02 
#>  5    22    22 psy1  103.  -2.26  -2.22 
#>  6    22    22 psy2   88.9 -1.13  -1.13 
#>  7    23    23 psy1  114.  -1.97  -1.95 
#>  8    23    23 psy2   94.1 -1.15  -1.15 
#>  9    24    24 psy1  103.  -2.53  -2.51 
#> 10    24    24 psy2   88.7 -1.21  -1.21 
#> # ℹ 152 more rows

# Get the panel test statistic
tidy(rfd, panel = TRUE)
#> # A tibble: 1 × 1
#>   gsadf_panel
#>         <dbl>
#> 1        2.11
# }
```
