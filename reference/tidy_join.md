# Tidy into a joint model

Tidy or augment and then join objects.

## Usage

``` r
tidy_join(x, y, ...)
```

## Arguments

- x:

  An object of class `obj`.

- y:

  An object of class `cv`.

- ...:

  Further arguments passed to methods.

## Value

A `tibble` joining the tidied statistics of `x` with the tidied critical
values of `y`; see the methods for the exact columns.

## Examples

``` r
# \donttest{
rsim <- radf(sim_data)
mc <- radf_mc_cv(nrow(sim_data), nrep = 200)
tidy_join(rsim, mc)
#> # A tibble: 45 × 5
#>    id    stat  tstat sig     crit
#>    <fct> <fct> <dbl> <fct>  <dbl>
#>  1 psy1  adf   -2.46 90    -0.424
#>  2 psy1  adf   -2.46 95    -0.114
#>  3 psy1  adf   -2.46 99     0.149
#>  4 psy1  sadf   1.95 90     0.854
#>  5 psy1  sadf   1.95 95     1.28 
#>  6 psy1  sadf   1.95 99     1.79 
#>  7 psy1  gsadf  5.19 90     1.62 
#>  8 psy1  gsadf  5.19 95     2.03 
#>  9 psy1  gsadf  5.19 99     2.56 
#> 10 psy2  adf   -2.86 90    -0.424
#> # ℹ 35 more rows
augment_join(rsim, mc)
#> # A tibble: 2,430 × 8
#>      key index id     data stat   tstat sig    crit
#>    <int> <dbl> <fct> <dbl> <fct>  <dbl> <fct> <dbl>
#>  1    20    20 psy1  104.  badf  -2.31  90    -0.44
#>  2    21    21 psy1  110.  badf  -2.36  90    -0.44
#>  3    22    22 psy1   98.1 badf  -2.49  90    -0.44
#>  4    23    23 psy1   91.0 badf  -2.26  90    -0.44
#>  5    24    24 psy1   80.4 badf  -1.63  90    -0.44
#>  6    25    25 psy1   69.2 badf  -0.815 90    -0.44
#>  7    26    26 psy1   72.3 badf  -0.960 90    -0.44
#>  8    27    27 psy1   67.7 badf  -0.693 90    -0.44
#>  9    28    28 psy1   69.1 badf  -0.771 90    -0.44
#> 10    29    29 psy1   65.4 badf  -0.609 90    -0.44
#> # ℹ 2,420 more rows
# }
```
