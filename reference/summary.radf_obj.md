# Summarizing `radf` models

`summary` method for radf models that consist of `radf_obj` and
`radf_cv`.

## Usage

``` r
# S3 method for class 'radf_obj'
summary(object, cv = NULL, ...)
```

## Arguments

- object:

  An object of class `radf_obj`. The output of
  [`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md).

- cv:

  An object of class `radf_cv`. The output of
  [`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md),
  [`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
  or
  [`radf_sb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sb_cv.md).

- ...:

  Further arguments passed to methods. Not used.

## Value

Returns a list of summary statistics, which include the estimated ADF,
SADF, and GSADF test statistics and the corresponding critical values

## Examples

``` r
# \donttest{
# Simulate bubble processes, compute the test statistics and critical values
rsim_data <- radf(sim_data)

# Summary, diagnostics and datestamp (default)
summary(rsim_data)
#> Using `radf_crit` for `cv`.
#> 
#> ── Summary (minw = 19, lag = 0) ────────────────── Monte Carlo (nboot = 2000) ──
#> 
#> psy1 :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`    `95`  `99`
#>   <fct> <dbl>  <dbl>   <dbl> <dbl>
#> 1 adf   -2.46 -0.413 -0.0812 0.652
#> 2 sadf   1.95  0.988  1.29   1.92 
#> 3 gsadf  5.19  1.71   1.97   2.57 
#> 
#> psy2 :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`    `95`  `99`
#>   <fct> <dbl>  <dbl>   <dbl> <dbl>
#> 1 adf   -2.86 -0.413 -0.0812 0.652
#> 2 sadf   7.88  0.988  1.29   1.92 
#> 3 gsadf  7.88  1.71   1.97   2.57 
#> 
#> evans :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`    `95`  `99`
#>   <fct> <dbl>  <dbl>   <dbl> <dbl>
#> 1 adf   -5.83 -0.413 -0.0812 0.652
#> 2 sadf   5.28  0.988  1.29   1.92 
#> 3 gsadf  5.99  1.71   1.97   2.57 
#> 
#> div :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`    `95`  `99`
#>   <fct> <dbl>  <dbl>   <dbl> <dbl>
#> 1 adf   -1.95 -0.413 -0.0812 0.652
#> 2 sadf   1.11  0.988  1.29   1.92 
#> 3 gsadf  1.34  1.71   1.97   2.57 
#> 
#> blan :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`    `95`  `99`
#>   <fct> <dbl>  <dbl>   <dbl> <dbl>
#> 1 adf   -5.15 -0.413 -0.0812 0.652
#> 2 sadf   3.93  0.988  1.29   1.92 
#> 3 gsadf 11.0   1.71   1.97   2.57 
#> 

# Summary, diagnostics and datestamp (wild bootstrap critical values)

wb <- radf_wb_cv(sim_data)

summary(rsim_data, cv = wb)
#> 
#> ── Summary (minw = 19, lag = 0) ──────────────── Wild Bootstrap (nboot = 500) ──
#> 
#> psy1 :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`    `99`
#>   <fct> <dbl>  <dbl>  <dbl>   <dbl>
#> 1 adf   -2.46 -0.559 -0.418 -0.0199
#> 2 sadf   1.95  1.51   1.93   2.96  
#> 3 gsadf  5.19  2.86   3.25   4.82  
#> 
#> psy2 :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`   `99`
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>
#> 1 adf   -2.86 -0.613 -0.490 -0.215
#> 2 sadf   7.88  3.06   3.82   5.35 
#> 3 gsadf  7.88  3.87   4.63   6.17 
#> 
#> evans :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`    `99`
#>   <fct> <dbl>  <dbl>  <dbl>   <dbl>
#> 1 adf   -5.83 -0.526 -0.290 -0.0429
#> 2 sadf   5.28  5.65   8.04  14.2   
#> 3 gsadf  5.99  8.45  10.3   14.2   
#> 
#> div :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`  `99`
#>   <fct> <dbl>  <dbl>  <dbl> <dbl>
#> 1 adf   -1.95 -0.529 -0.168 0.504
#> 2 sadf   1.11  1.000  1.35  2.14 
#> 3 gsadf  1.34  1.79   2.07  2.60 
#> 
#> blan :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`    `95`   `99`
#>   <fct> <dbl>  <dbl>   <dbl>  <dbl>
#> 1 adf   -5.15 -0.361 -0.0142  0.619
#> 2 sadf   3.93  3.13   4.27    6.85 
#> 3 gsadf 11.0   5.71   7.01   10.2  
#> 

# summary() reports the same numbers autoplot() draws
autoplot(rsim_data, cv = wb)

# }
```
