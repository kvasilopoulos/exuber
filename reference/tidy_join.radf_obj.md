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
#>    id    stat  tstat sig      crit
#>    <fct> <fct> <dbl> <fct>   <dbl>
#>  1 psy1  adf   -2.46 90    -0.632 
#>  2 psy1  adf   -2.46 95    -0.480 
#>  3 psy1  adf   -2.46 99    -0.0686
#>  4 psy1  sadf   1.95 90     1.60  
#>  5 psy1  sadf   1.95 95     2.09  
#>  6 psy1  sadf   1.95 99     2.87  
#>  7 psy1  gsadf  5.19 90     2.72  
#>  8 psy1  gsadf  5.19 95     3.22  
#>  9 psy1  gsadf  5.19 99     4.48  
#> 10 psy2  adf   -2.86 90    -0.637 
#> # ℹ 35 more rows

# summary() and diagnostics() are themselves built on top of tidy_join()
summary(rsim_data, cv = cv)
#> 
#> ── Summary (minw = 20, lag = 0) ──────────────── Wild Bootstrap (nboot = 500) ──
#> 
#> psy1 :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`    `99`
#>   <fct> <dbl>  <dbl>  <dbl>   <dbl>
#> 1 adf   -2.46 -0.632 -0.480 -0.0686
#> 2 sadf   1.95  1.60   2.09   2.87  
#> 3 gsadf  5.19  2.72   3.22   4.48  
#> 
#> psy2 :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`   `99`
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>
#> 1 adf   -2.86 -0.637 -0.529 -0.286
#> 2 sadf   7.88  2.82   3.89   5.35 
#> 3 gsadf  7.88  3.85   4.92   6.12 
#> 
#> evans :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`    `99`
#>   <fct> <dbl>  <dbl>  <dbl>   <dbl>
#> 1 adf   -5.83 -0.615 -0.353 -0.0438
#> 2 sadf  -2.73  5.48   7.68  12.3   
#> 3 gsadf  5.47  7.73  10.2   14.6   
#> 
#> div :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`    `95`  `99`
#>   <fct> <dbl>  <dbl>   <dbl> <dbl>
#> 1 adf   -1.95 -0.384 -0.0161 0.578
#> 2 sadf   1.11  0.925  1.27   1.78 
#> 3 gsadf  1.11  1.77   2.03   2.80 
#> 
#> blan :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`   `99`
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>
#> 1 adf   -5.15 -0.238 0.0611  0.448
#> 2 sadf   3.93  3.07  4.11    6.25 
#> 3 gsadf 11.0   5.88  7.49   10.8  
#> 
# }
```
