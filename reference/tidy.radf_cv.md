# Tidy a `radf_cv` object

Summarizes information about `radf_cv` object.

## Usage

``` r
# S3 method for class 'radf_cv'
tidy(x, format = c("wide", "long"), ...)

# S3 method for class 'radf_cv'
augment(x, format = c("wide", "long"), trunc = TRUE, ...)
```

## Arguments

- x:

  An object of class `radf_cv`.

- format:

  Long or wide format (default = "wide").

- ...:

  Further arguments passed to methods. Not used.

- trunc:

  Whether to remove the period of the minimum window from the plot
  (default = TRUE).

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
#>   sig      adf  sadf gsadf
#>   <fct>  <dbl> <dbl> <dbl>
#> 1 90    -0.477 0.956  1.67
#> 2 95    -0.101 1.27   1.99
#> 3 99     0.635 1.90   2.60

# Get the critical value sequences
augment(mc)
#> # A tibble: 243 × 4
#>      key sig    badf    bsadf
#>    <int> <fct> <dbl>    <dbl>
#>  1    20 90    -0.44 -0.372  
#>  2    20 95    -0.08 -0.00433
#>  3    20 99     0.6   0.658  
#>  4    21 90    -0.44 -0.192  
#>  5    21 95    -0.08  0.207  
#>  6    21 99     0.6   0.843  
#>  7    22 90    -0.44 -0.0709 
#>  8    22 95    -0.08  0.237  
#>  9    22 99     0.6   1.00   
#> 10    23 90    -0.44  0.00989
#> # ℹ 233 more rows
# }
```
