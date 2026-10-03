# WLS/Kernel-Volatility Bubble Statistic (SBZ)

`radf_sbz` computes the WLS (kernel-volatility-weighted) recursive
sup-ADF statistic of Harvey, Leybourne & Zu (2019), called `supBZ` in
their notation, with `wls_dfstat_grid()` (internal). It returns the same
shape as
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md): the
scalars `adf`, `sadf` and `gsadf` plus the full recursive paths `badf`
and `bsadf`. The result therefore carries the `radf_obj` class, and the
full [`summary()`](https://rdrr.io/r/base/summary.html),
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md),
`tidy` and `autoplot` pipeline works with it when it is paired with
[`radf_sbz_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md).

## Usage

``` r
radf_sbz(data, minw = NULL, kernel = c("gaussian", "uniform"), h = NULL)
```

## Arguments

- data:

  A univariate or multivariate numeric time series object, a numeric
  vector or matrix, or a data.frame. A column may have leading or
  trailing `NA` values, which describes an unbalanced panel in which
  series enter or exit the sample at different times. Those periods are
  filled with `NA` in `badf` and `bsadf` and excluded from the `adf`,
  `sadf` and `gsadf` of that series. Interior `NA` values (a gap in the
  middle of a series) are not supported. When any series is padded in
  this way, the panel statistics (`bsadf_panel` and `gsadf_panel`) are
  not available, and the function returns `NA` for them with a warning.

- minw:

  A positive integer. The minimum window size (default = \\(0.01 +
  1.8/\sqrt{T})T\\, where T denotes the sample size).

- kernel:

  Kernel for the spot-volatility estimator (eq. 6 of Harvey, Leybourne &
  Zu 2019), `"gaussian"` (default, as in the paper) or `"uniform"`.

- h:

  Bandwidth for the spot-volatility estimator. The default is
  leave-one-out cross-validation over the search range of the paper.

## Value

An object of class `radf_sbz_obj`/`radf_obj`: a list with `adf`, `sadf`
and `gsadf` (one value per series) and `badf` and `bsadf` (matrices, one
column per series).

## Details

The bundled
[`radf_sbz_union`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md)
combines this statistic with the classic `supDF` statistic into a
bootstrap-calibrated union test. `supBZ` alone needs a bootstrap only to
be tested and not to be *defined*, so it splits into a statistic and a
critical-value function, as most of exuber does.

## Note

The test needs the critical values from
[`radf_sbz_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md),
and neither
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
nor
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
applies. The null distribution of `supBZ` depends on the WLS weighting,
so it needs its own critical-value function, which is data-dependent and
uses a wild bootstrap, for the same reason as
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md) and
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md).

## Status

**\[experimental\]**

## References

Harvey, D. I., Leybourne, S. J., & Zu, Y. (2019). Testing explosive
bubbles with time-varying volatility. Econometric Reviews, 38(10),
1131-1151.

## See also

[`radf_sbz_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md)
for critical values, and
[`radf_sbz_union`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md)
for the main bootstrap union-of-rejections test of the paper, against
the classic `supDF` statistic.

Other volatility-robust tests:
[`cusum_test()`](https://kvasilopoulos.github.io/exuber/reference/cusum_test.md),
[`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md),
[`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md),
[`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md),
[`radf_sign_dm()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md),
[`radf_tt()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md),
[`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md)

## Examples

``` r
# \donttest{
# Volatility triples at t = 100, then a strong explosive regime (rho = 1.03)
# runs from t = 120 to the sample end. The kernel-volatility weighting of supBZ
# costs enough power that the milder default bubble of sim_psy1() does not clear it
y <- sim_psy1(n = 200, te = 120, tf = 200, c = 0.03, alpha = 0, seed = 1,
  e = sim_vol_break(199))
res <- radf_sbz(y, minw = 20)
print(res)
#> 
#> ── radf_sbz (minw = 20, kernel = gaussian) ─────────────────────────────────────
#> 
#>    series    adf   sadf  gsadf
#>   series1  4.829  4.829  5.287
#> 

cv <- radf_sbz_cv(y, minw = 20, nboot = 200, seed = 1)
summary(res, cv = cv)
#> 
#> ── Summary (minw = 20, lag = 0) ────────── Wild Bootstrap (SBZ) (nboot = 200) ──
#> 
#> series1 :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf    4.83 0.948  1.65  2.65
#> 2 sadf   4.83 2.24   2.49  3.26
#> 3 gsadf  5.29 2.77   3.00  3.58
#> 
tidy(res, cv = cv)
#> # A tibble: 1 × 4
#>   id        adf  sadf gsadf
#>   <fct>   <dbl> <dbl> <dbl>
#> 1 series1  4.83  4.83  5.29
datestamp(res, cv = cv)
#> 
#> ── Datestamp (min_duration = 0) ──────────────────────── Wild Bootstrap (SBZ) ──
#> 
#> series1 :
#>   Start Peak End Duration   Signal Ongoing
#> 1   129  129 130        1 positive   FALSE
#> 2   132  132 133        1 positive   FALSE
#> 3   134  134 135        1 positive   FALSE
#> 4   172  200 200       29 positive    TRUE
#> 
autoplot(res, cv = cv)

# }
```
