# Time-Transformed Test for Explosive Bubbles under Non-stationary Volatility

`radf_tt` computes the STADF and GSTADF test statistics of Kurozumi,
Skrobotov & Tsarev, a heteroskedasticity-robust alternative to
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) that
needs no bootstrap. It time-deforms the series with a nonparametric
estimate of its variance profile, after which the usual asymptotic
recursive sup-ADF critical values for homoskedastic errors apply.

## Usage

``` r
radf_tt(data, minw = NULL, kernel = c("uniform", "gaussian"), h = NULL)
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

  Kernel used in the local variance-profile regression, `"uniform"`
  (default, as in the simulations of the paper) or `"gaussian"`.

- h:

  Bandwidth for the variance-profile kernel regression. The default is
  `T^(-2/5)`, the midpoint on the log scale of the cross-validation
  search range \\\[T^{-0.5}, T^{-0.3}\]\\ of the paper.

## Value

An object of class `radf_tt_obj`/`radf_obj`: the same
`adf`/`badf`/`sadf`/`bsadf`/`gsadf` list as for
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md),
computed on the time-transformed series. It works with
[`summary()`](https://rdrr.io/r/base/summary.html),
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
when paired with
[`radf_tt_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md).

## Details

We recommend
[`radf_tt_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md)
for the critical values. They are pivotal (asymptotically free of the
volatility process), so they do not have to be recomputed for each
dataset, unlike a bootstrap. The wild bootstrap of Harvey, Leybourne,
Sollis & Taylor in
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
is a bootstrap-based alternative. Consider it if non-pivotality or the
finite-sample robustness of the bootstrap is a specific concern.

## Note

The result carries the `radf_obj` class. Since 2026-08-18 the full
[`summary()`](https://rdrr.io/r/base/summary.html),
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md),
`tidy` and `autoplot` pipeline works, because
[`radf_tt_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md)
now computes the time-varying `badf_cv` and `bsadf_cv` boundary that the
last two need, and not only the three scalar critical values that
[`summary()`](https://rdrr.io/r/base/summary.html) uses. See
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md).

## Status

**\[experimental\]**

## References

Kurozumi, E., Skrobotov, A., & Tsarev, A. (2024). Time-Transformed Test
for Bubbles under Non-stationary Volatility. Journal of Financial
Econometrics.
[doi:10.1093/jjfinec/nbae026](https://doi.org/10.1093/jjfinec/nbae026)

## See also

[`radf_tt_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md)
for the pivotal asymptotic critical values that need no bootstrap, and
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
for the bootstrap-based alternative (Harvey, Leybourne, Sollis &
Taylor).

Other volatility-robust tests:
[`cusum_test()`](https://kvasilopoulos.github.io/exuber/reference/cusum_test.md),
[`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md),
[`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md),
[`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md),
[`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md),
[`radf_sign_dm()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md),
[`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md)

## Examples

``` r
# \donttest{
# Volatility triples half-way through the sample. This is the case of
# non-stationary volatility that this test is built for, and plain radf()
# over-rejects here
y <- sim_psy1(n = 200, seed = 1, e = sim_vol_break(199))
res <- radf_tt(y, minw = 20)
print(res)
#> 
#> ── radf_tt (minw = 20, kernel = uniform) ───────────────────────────────────────
#> 
#>    series      adf   sadf  gsadf
#>   series1  -0.9972  3.275  4.229
#> 

cv <- radf_tt_cv(n = 200, minw = 20)
summary(res, cv = cv)
#> 
#> ── Summary (minw = 20, lag = 0) ────────── Time-Transformed MC (nboot = 2000) ──
#> 
#> series1 :
#> # A tibble: 3 × 5
#>   stat   tstat  `90`  `95`  `99`
#>   <fct>  <dbl> <dbl> <dbl> <dbl>
#> 1 adf   -0.997 0.833  1.21  1.97
#> 2 sadf   3.27  2.31   2.65  3.35
#> 3 gsadf  4.23  3.27   3.65  4.42
#> 
tidy(res, cv = cv)
#> # A tibble: 1 × 4
#>   id         adf  sadf gsadf
#>   <fct>    <dbl> <dbl> <dbl>
#> 1 series1 -0.997  3.27  4.23
datestamp(res, cv = cv)
#> 
#> ── Datestamp (min_duration = 0) ───────────────────────── Time-Transformed MC ──
#> 
#> series1 :
#>   Start Peak End Duration   Signal Ongoing
#> 1    21   38  89       68 negative   FALSE
#> 2   148  148 149        1 positive   FALSE
#> 
autoplot(res, cv = cv)

# }
```
