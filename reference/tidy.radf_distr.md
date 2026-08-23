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
# \donttest{
mdist <- radf_mc_distr(n = 100, nrep = 1000)

tidy(mdist)
#> # A tibble: 1,000 × 3
#>       adf    sadf   gsadf
#>     <dbl>   <dbl>   <dbl>
#>  1 -2.06  -0.139   1.51  
#>  2 -1.76  -0.142   0.794 
#>  3 -0.859  0.136   2.27  
#>  4 -1.24  -0.564   0.655 
#>  5 -0.784  0.735   1.93  
#>  6 -3.11  -0.872   0.0878
#>  7 -1.08   0.399   1.71  
#>  8 -1.64  -0.0483 -0.0108
#>  9 -1.78  -0.177   0.740 
#> 10 -0.785  0.316   0.529 
#> # ℹ 990 more rows

# Plot the resulting statistic distributions
autoplot(mdist)

# }
```
