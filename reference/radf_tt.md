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
  1.8/\sqrt(T))T\\, where T denotes the sample size).

- kernel:

  Kernel used in the local variance-profile regression, `"uniform"`
  (default, as in the paper's simulations) or `"gaussian"`.

- h:

  Bandwidth for the variance-profile kernel regression. Default
  `T^(-2/5)`, the midpoint (on the log scale) of the paper's
  cross-validation search range \\\[T^{-0.5}, T^{-0.3}\]\\.

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

Carries the `radf_obj` class, so
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) work, but
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/`autoplot`
do not:
[`radf_tt_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md)
only computes the three scalar critical values, not the time-varying
boundary those two need – see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md).

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

## Examples

``` r
# \donttest{
res <- radf_tt(sim_data, minw = 20)
print(res)
#> 
#> ── radf_tt (minw = 20, kernel = uniform) ───────────────────────────────────────
#> 
#>   series      adf    sadf  gsadf
#>     psy1  -1.0366  1.2750  2.204
#>     psy2  -0.8600  2.5960  3.505
#>    evans  -1.3349  1.6556  1.883
#>      div   0.7217  2.3440  2.344
#>     blan  -1.3363  0.4998  1.541
#> 

cv <- radf_tt_cv(n = 100, minw = 20)
summary(res, cv = cv)
#> 
#> ── Summary (minw = 20, lag = 0) ────────── Time-Transformed MC (nboot = 2000) ──
#> 
#> psy1 :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf   -1.04 0.970  1.44  2.08
#> 2 sadf   1.27 2.20   2.56  3.15
#> 3 gsadf  2.20 2.85   3.15  3.88
#> 
#> psy2 :
#> # A tibble: 3 × 5
#>   stat   tstat  `90`  `95`  `99`
#>   <fct>  <dbl> <dbl> <dbl> <dbl>
#> 1 adf   -0.860 0.970  1.44  2.08
#> 2 sadf   2.60  2.20   2.56  3.15
#> 3 gsadf  3.50  2.85   3.15  3.88
#> 
#> evans :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf   -1.33 0.970  1.44  2.08
#> 2 sadf   1.66 2.20   2.56  3.15
#> 3 gsadf  1.88 2.85   3.15  3.88
#> 
#> div :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf   0.722 0.970  1.44  2.08
#> 2 sadf  2.34  2.20   2.56  3.15
#> 3 gsadf 2.34  2.85   3.15  3.88
#> 
#> blan :
#> # A tibble: 3 × 5
#>   stat   tstat  `90`  `95`  `99`
#>   <fct>  <dbl> <dbl> <dbl> <dbl>
#> 1 adf   -1.34  0.970  1.44  2.08
#> 2 sadf   0.500 2.20   2.56  3.15
#> 3 gsadf  1.54  2.85   3.15  3.88
#> 
tidy(res, cv = cv)
#> # A tibble: 5 × 4
#>   id       adf  sadf gsadf
#>   <fct>  <dbl> <dbl> <dbl>
#> 1 psy1  -1.04  1.27   2.20
#> 2 psy2  -0.860 2.60   3.50
#> 3 evans -1.33  1.66   1.88
#> 4 div    0.722 2.34   2.34
#> 5 blan  -1.34  0.500  1.54
# }
```
