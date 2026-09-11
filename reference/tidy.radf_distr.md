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
#>       adf    sadf  gsadf
#>     <dbl>   <dbl>  <dbl>
#>  1 -1.36   0.471   1.40 
#>  2 -2.08  -0.366   1.27 
#>  3 -3.44  -1.17   -0.490
#>  4 -0.637 -0.0144  0.232
#>  5 -1.43  -0.170   1.06 
#>  6 -2.12   0.0400  1.25 
#>  7 -0.898  0.752   1.19 
#>  8 -3.37  -0.802  -0.474
#>  9 -1.34  -0.0894  0.702
#> 10 -2.56   0.0138  0.757
#> # ℹ 990 more rows

# Plot the resulting statistic distributions
autoplot(mdist)

# }
```
