# WLS/Kernel-Volatility Bubble Statistic (SBZ)

`radf_sbz` computes the WLS (kernel-volatility-weighted) recursive
sup-ADF statistic of Harvey, Leybourne & Zu (2019) – `supBZ` in their
own notation – via `wls_dfstat_grid()` (internal), returning the same
shape [`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
itself does (`adf`/`sadf`/`gsadf` scalars plus the full `badf`/`bsadf`
recursive paths), so it carries the `radf_obj` class and the full
[`summary()`](https://rdrr.io/r/base/summary.html)/
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/`tidy`/`autoplot`
pipeline works, paired with
[`radf_sbz_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md).

## Usage

``` r
radf_sbz(data, minw = NULL, kernel = c("gaussian", "uniform"), h = NULL)
```

## Arguments

- data:

  A univariate or multivariate numeric time series object, a numeric
  vector or matrix, or a data.frame. A column may have leading and/or
  trailing `NA` values (an uneven/unbalanced panel where series enter or
  exit the sample at different times) – those periods are filled with
  `NA` in `badf`/`bsadf` and excluded from that series' `adf`/`sadf`/
  `gsadf`. Interior `NA` values (a gap in the middle of a series) are
  not supported. When any series is padded this way, the panel statistic
  (`bsadf_panel`/`gsadf_panel`) is not available and is returned as
  `NA`, with a warning.

- minw:

  A positive integer. The minimum window size (default = \\(0.01 +
  1.8/\sqrt(T))T\\, where T denotes the sample size).

- kernel:

  Kernel for the spot-volatility estimator (eq. 6 of Harvey, Leybourne &
  Zu 2019), `"gaussian"` (default, as in the paper) or `"uniform"`.

- h:

  Bandwidth for the spot-volatility estimator. Default: leave-one-out
  cross-validation over the paper's own search range.

## Value

An object of class `radf_sbz_obj`/`radf_obj`: a list with `adf`, `sadf`,
`gsadf` (one value per series) and `badf`, `bsadf` (matrices, one column
per series).

## Details

Unlike the bundled
[`radf_sbz_union`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md)
(which combines this with the classic `supDF` statistic into a
bootstrap-calibrated union test), `supBZ` alone needs no bootstrap to be
*defined* – only to be tested – so it splits into a statistic and a
critical-value function the way most of exuber does.

## Note

Needs
[`radf_sbz_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md)
for critical values, not
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
or
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
– `supBZ`'s own null distribution depends on the WLS weighting, so it
needs its own (data-dependent, wild-bootstrap) critical value function,
same reasoning as
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)/[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md).

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
for the paper's own headline bootstrap union-of-rejections test against
the classic `supDF` statistic.

## Examples

``` r
# \donttest{
# Volatility triples at t = 100, then a strong explosive regime (rho = 1.03)
# from t = 120 to the sample end: supBZ's kernel-volatility weighting trades
# away enough power that sim_psy1()'s default, milder bubble doesn't clear it
y <- sim_psy1(n = 200, te = 120, tf = 200, c = 0.03, alpha = 0, seed = 1,
  e = sim_vol_break(199))
res <- radf_sbz(y, minw = 20)
print(res)
#> 
#> ── radf (minw = 20, lag = 0) ───────────────────────────────────────────────────
#> 
#>        id    adf   sadf  gsadf
#>   series1  4.829  4.829  5.287
#> 
#> [1] gsadf_panel
#> <0 rows> (or 0-length row.names)
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
