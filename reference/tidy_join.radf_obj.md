# Tidy into a joint model

Tidy or augment and then join objects of class `radf_obj` and `radf_cv`.
The object of reference is the `radf_cv`. For example, if panel critical
values are provided the function will return the panel test statistic.

## Usage

``` r
# S3 method for class 'radf_obj'
tidy_join(x, y = NULL, ...)
```

## Arguments

- x:

  An object of class `radf_obj`.

- y:

  An object of class `radf_cv`. The output will depend on the type of
  critical value.

- ...:

  Further arguments passed to methods. Not used.

## Details

`tidy_join` also calls `augment_join` when `cv` is of class `sb_cv`.

## Examples

``` r
# \donttest{
rsim_data <- radf(sim_data, minw = 20)
cv <- radf_wb_cv(sim_data, minw = 20)

# One row per series/statistic, statistic and critical value side by side
tidy_join(rsim_data, cv)
#> # A tibble: 45 × 5
#>    id    stat  tstat sig     crit
#>    <fct> <fct> <dbl> <fct>  <dbl>
#>  1 psy1  adf   -2.46 90    -0.472
#>  2 psy1  adf   -2.46 95    -0.213
#>  3 psy1  adf   -2.46 99     0.403
#>  4 psy1  sadf   1.95 90     1.61 
#>  5 psy1  sadf   1.95 95     2.29 
#>  6 psy1  sadf   1.95 99     3.48 
#>  7 psy1  gsadf  5.19 90     2.80 
#>  8 psy1  gsadf  5.19 95     3.32 
#>  9 psy1  gsadf  5.19 99     4.43 
#> 10 psy2  adf   -2.86 90    -0.661
#> # ℹ 35 more rows

# summary() and diagnostics() are themselves built on top of tidy_join()
summary(rsim_data, cv = cv)
#> 
#> ── Summary (minw = 20, lag = 0) ──────────────── Wild Bootstrap (nboot = 500) ──
#> 
#> psy1 :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`  `99`
#>   <fct> <dbl>  <dbl>  <dbl> <dbl>
#> 1 adf   -2.46 -0.472 -0.213 0.403
#> 2 sadf   1.95  1.61   2.29  3.48 
#> 3 gsadf  5.19  2.80   3.32  4.43 
#> 
#> psy2 :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`   `99`
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>
#> 1 adf   -2.86 -0.661 -0.495 -0.291
#> 2 sadf   7.88  2.99   3.60   5.05 
#> 3 gsadf  7.88  3.80   4.58   5.86 
#> 
#> evans :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`   `99`
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>
#> 1 adf   -5.83 -0.623 -0.445 -0.218
#> 2 sadf  -2.73  4.92   7.50  11.5  
#> 3 gsadf  5.47  7.45   9.38  14.1  
#> 
#> div :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`    `95`  `99`
#>   <fct> <dbl>  <dbl>   <dbl> <dbl>
#> 1 adf   -1.95 -0.338 -0.0421 0.548
#> 2 sadf   1.11  1.00   1.29   1.91 
#> 3 gsadf  1.11  1.76   2.09   2.64 
#> 
#> blan :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`   `99`
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>
#> 1 adf   -5.15 -0.326 0.0951  0.383
#> 2 sadf   3.93  2.77  4.01    6.28 
#> 3 gsadf 11.0   5.99  7.40   12.3  
#> 
# }
```
