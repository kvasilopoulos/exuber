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
#> 1 psy1  -2.78 -0.713 -0.187
#> 2 psy2  -2.58  4.03   4.87 

# Get the test statisticsequences
augment(rfd)
#> # A tibble: 162 × 6
#>      key index id     data  badf bsadf
#>    <int> <dbl> <chr> <dbl> <dbl> <dbl>
#>  1    20    20 psy1  109.  -1.76 -1.76
#>  2    20    20 psy2   72.8 -1.10 -1.10
#>  3    21    21 psy1  108.  -1.71 -1.62
#>  4    21    21 psy2   79.4 -1.27 -1.03
#>  5    22    22 psy1   96.5 -2.27 -2.15
#>  6    22    22 psy2   78.4 -1.30 -1.06
#>  7    23    23 psy1  100.  -2.34 -2.22
#>  8    23    23 psy2   84.8 -1.37 -1.11
#>  9    24    24 psy1  111.  -2.04 -1.95
#> 10    24    24 psy2   89.8 -1.36 -1.09
#> # ℹ 152 more rows

# Get the panel test statistic
tidy(rfd, panel = TRUE)
#> # A tibble: 1 × 1
#>   gsadf_panel
#>         <dbl>
#> 1        2.02
# }
```
