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
#>  1 psy1  adf   -2.46 90    -0.584
#>  2 psy1  adf   -2.46 95    -0.432
#>  3 psy1  adf   -2.46 99    -0.154
#>  4 psy1  sadf   1.95 90     1.49 
#>  5 psy1  sadf   1.95 95     1.89 
#>  6 psy1  sadf   1.95 99     2.77 
#>  7 psy1  gsadf  5.19 90     2.86 
#>  8 psy1  gsadf  5.19 95     3.18 
#>  9 psy1  gsadf  5.19 99     4.73 
#> 10 psy2  adf   -2.86 90    -0.638
#> # ℹ 35 more rows

# summary() and diagnostics() are themselves built on top of tidy_join()
summary(rsim_data, cv = cv)
#> 
#> ── Summary (minw = 20, lag = 0) ──────────────── Wild Bootstrap (nboot = 500) ──
#> 
#> psy1 :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`   `99`
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>
#> 1 adf   -2.46 -0.584 -0.432 -0.154
#> 2 sadf   1.95  1.49   1.89   2.77 
#> 3 gsadf  5.19  2.86   3.18   4.73 
#> 
#> psy2 :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`   `99`
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>
#> 1 adf   -2.86 -0.638 -0.494 -0.184
#> 2 sadf   7.88  3.19   4.11   6.06 
#> 3 gsadf  7.88  4.10   4.89   6.23 
#> 
#> evans :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`   `99`
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>
#> 1 adf   -5.83 -0.531 -0.327 -0.175
#> 2 sadf  -2.73  5.08   7.13  12.4  
#> 3 gsadf  5.47  7.80   9.60  13.8  
#> 
#> div :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`    `95`  `99`
#>   <fct> <dbl>  <dbl>   <dbl> <dbl>
#> 1 adf   -1.95 -0.370 -0.0382 0.809
#> 2 sadf   1.11  0.955  1.25   1.66 
#> 3 gsadf  1.11  1.69   2.04   2.75 
#> 
#> blan :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`    `95`   `99`
#>   <fct> <dbl>  <dbl>   <dbl>  <dbl>
#> 1 adf   -5.15 -0.409 -0.0492  0.292
#> 2 sadf   3.93  3.18   4.54    6.62 
#> 3 gsadf 11.0   6.17   7.77   11.7  
#> 
# }
```
