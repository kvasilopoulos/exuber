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
#> 1 adf   -2.46 -0.620 -0.457 -0.0626
#> 2 sadf   1.95  1.30   1.74   2.84  
#> 3 gsadf  5.19  2.58   3.17   4.82  
#> 
#> psy2 :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`   `99`
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>
#> 1 adf   -2.86 -0.602 -0.420 -0.246
#> 2 sadf   7.88  3.23   4.00   5.89 
#> 3 gsadf  7.88  4.11   4.91   6.95 
#> 
#> evans :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`   `99`
#>   <fct> <dbl>  <dbl>  <dbl>  <dbl>
#> 1 adf   -5.83 -0.555 -0.411 -0.109
#> 2 sadf   5.28  5.03   6.53  11.0  
#> 3 gsadf  5.99  7.72   9.54  13.7  
#> 
#> div :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`    `95`  `99`
#>   <fct> <dbl>  <dbl>   <dbl> <dbl>
#> 1 adf   -1.95 -0.380 0.00187 0.361
#> 2 sadf   1.11  0.899 1.23    1.73 
#> 3 gsadf  1.34  1.74  2.05    2.87 
#> 
#> blan :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`    `95`   `99`
#>   <fct> <dbl>  <dbl>   <dbl>  <dbl>
#> 1 adf   -5.15 -0.316 -0.0814  0.270
#> 2 sadf   3.93  2.87   3.87    6.52 
#> 3 gsadf 11.0   6.11   7.55   10.8  
#> 

# summary() reports the same numbers autoplot() draws
autoplot(rsim_data, cv = wb)

# }
```
