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
#> Error in full_join(tidy(x, format = "long"), tidy(y, format = "long"),     by = c("stat", join_by), relationship = "many-to-many"): Join columns in `y` must be present in the data.
#> ✖ Problem with `id`.
# }
```
