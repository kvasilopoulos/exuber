# Time-Transformed Test for Explosive Bubbles under Non-stationary Volatility

`radf_tt` computes the STADF/GSTADF test statistics of Kurozumi,
Skrobotov & Tsarev, a heteroskedasticity-robust alternative to
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) that
requires no bootstrap: the series is time-deformed using a nonparametric
estimate of its variance profile, after which the usual (asymptotic,
homoskedastic) recursive sup-ADF critical values apply.

## Usage

``` r
radf_tt(data, minw = NULL, kernel = c("uniform", "gaussian"), h = NULL)
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
  1.8/\sqrt{T})T\\, where T denotes the sample size).

- kernel:

  Kernel used in the local variance-profile regression, `"uniform"`
  (default, as in the paper's simulations) or `"gaussian"`.

- h:

  Bandwidth for the variance-profile kernel regression. Default
  `T^(-2/5)`, the midpoint (on the log scale) of the paper's
  cross-validation search range \\\[T^{-0.5}, T^{-0.3}\]\\.

## Value

An object of class `radf_tt_obj`/`radf_obj`: the same
`adf`/`badf`/`sadf`/`bsadf`/`gsadf` list as
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md),
computed on the time-transformed series, so it plugs into
[`summary()`](https://rdrr.io/r/base/summary.html)/[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/[`tidy()`](https://generics.r-lib.org/reference/tidy.html)/[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
paired with
[`radf_tt_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md).

## Details

For critical values, use
[`radf_tt_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md)
as the primary recommendation: it is pivotal (asymptotically free of the
volatility process), so it does not need to be recomputed per dataset,
unlike a bootstrap.
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
(Harvey, Leybourne, Sollis & Taylor's wild bootstrap) is a
bootstrap-based alternative, worth considering if
non-pivotality/finite-sample bootstrap robustness is a specific concern.

## Note

Carries the `radf_obj` class and, as of 2026-08-18, its full
[`summary()`](https://rdrr.io/r/base/summary.html)/[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/`tidy`/`autoplot`
pipeline works –
[`radf_tt_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md)
now computes the time-varying `badf_cv`/`bsadf_cv` boundary those last
two need, not just the three scalar critical values
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
for the (pivotal, bootstrap-free) asymptotic critical values, and
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
for the bootstrap-based alternative (Harvey, Leybourne, Sollis &
Taylor).

Other volatility-robust tests:
[`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md),
[`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md),
[`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md),
[`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md),
[`radf_sign_dm()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md),
[`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md)

## Examples

``` r
# \donttest{
# Volatility triples half-way through the sample: the non-stationary-volatility
# case this test is built for (plain radf() over-rejects here)
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
