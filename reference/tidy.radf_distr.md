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
#>        adf   sadf gsadf
#>      <dbl>  <dbl> <dbl>
#>  1 -2.78    1.59  1.92 
#>  2 -0.438   0.122 1.15 
#>  3 -1.12    0.737 1.30 
#>  4  0.105   0.552 1.31 
#>  5 -1.60   -0.310 0.598
#>  6 -1.63   -0.430 0.515
#>  7 -0.684  -0.401 0.394
#>  8 -0.0450  0.101 0.192
#>  9 -0.654   0.588 1.39 
#> 10 -0.999   1.30  1.30 
#> # ℹ 990 more rows

# Plot the resulting statistic distributions
autoplot(mdist)

# }
```
