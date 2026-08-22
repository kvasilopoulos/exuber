# SBZ Weighted Least Squares Bubble Test with Union-of-Rejections

`radf_sbz_union` performs the HLST (2016) wild bootstrap – the same
algorithm as
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
– *jointly* on the classic sup-ADF statistic (`supDF`, i.e.
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s
`sadf`) and the WLS/kernel-volatility statistic `supBZ` of Harvey,
Leybourne & Zu (2019), and combines them into the paper's
union-of-rejections statistic `U`. supBZ can have substantially higher
power than supDF under many time-varying-volatility patterns, at the
cost of lower power under others (e.g. upward volatility trends); `U` is
designed to capture whichever of the two is more powerful for a given
series.

## Usage

``` r
radf_sbz_union(
  data,
  minw = NULL,
  nboot = 499L,
  kernel = c("gaussian", "uniform"),
  h = NULL,
  seed = NULL
)

# S3 method for class 'radf_sbz_union'
autoplot(object, sig_lvl = 95, ...)
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

- nboot:

  A positive integer. Number of bootstraps (default = 500L).

- kernel:

  Kernel for the spot-volatility estimator (eq. 6), `"gaussian"`
  (default, as in the paper) or `"uniform"`.

- h:

  Bandwidth for the spot-volatility estimator. Default: leave-one-out
  cross-validation over the paper's own search range.

- seed:

  An object specifying if and how the random number generator (rng)
  should be initialized. Either NULL or an integer will be used in a
  call to `set.seed` before simulation. If set, the value is saved as
  "seed" attribute of the returned value. The default, NULL, will not
  change rng state, and return .Random.seed as the "seed" attribute.
  Results are reproducible across the parallel and non-parallel option
  when the same seed is used.

- object:

  A `radf_sbz_union` object.

- sig_lvl:

  Significance level to plot the critical value at, one of `90`, `95`
  (default), `99`.

- ...:

  Further arguments passed to methods. Not used.

## Value

A list with bootstrap p-values (`p_supDF`, `p_supBZ`, `p_U`) and
critical values (`supDF_cv`, `supBZ_cv`, `U_cv`) for each series.

## Details

`U`'s value itself – not just its significance – is defined using a
bootstrap-calibrated scaling ratio between `supDF` and `supBZ`'s own
95\\ the joint bootstrap needs `supDF`/`supBZ` computed from the *same*
resampled series each replicate for the union's size guarantee (the
paper's Theorem 3) to hold. That coupling is why this stays one bundled
function rather than splitting into a statistic and a critical-value
function the way most of exuber does – unlike `U`, `supBZ` alone has no
such coupling, so it does split that way: see
[`radf_sbz`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)/[`radf_sbz_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md)
for the supBZ-only route, with the usual
[`summary()`](https://rdrr.io/r/base/summary.html)/[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/
`tidy`/`autoplot` pipeline.

## Note

This function bundles the statistic and its critical values in a single
call – there is no separate un-cv'd statistic function and no other
critical-value function to pair it with, unlike
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)/
[`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
(`U`'s own value structurally requires the bootstrap, see Details).

Returns its own class (not `radf_obj`), so it does not plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/`\link{datestamp}`/`tidy`/`autoplot`
– prints its own statistic/critical-value summary (bundles the test
statistic and its critical value in one object), but has its own
`autoplot` method (a per-series comparison of `supDF`/`supBZ`/`U`
against their critical values) – see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the full picture of which functions do and don't fit the shared
pipeline.

## Status

**\[experimental\]**

## References

Harvey, D. I., Leybourne, S. J., & Zu, Y. (2019). Testing explosive
bubbles with time-varying volatility. Econometric Reviews, 38(10),
1131-1151.

## See also

[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
for the underlying (supDF-only) wild bootstrap,
[`radf_sbz`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)/[`radf_sbz_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md)
for the supBZ-only route with full pipeline support, and
[`radf_tt`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md)
for a bootstrap-free heteroskedasticity-robust alternative.

## Examples

``` r
# \donttest{
res <- radf_sbz_union(sim_data, nboot = 200)
print(res)
#> 
#> ── radf_sbz_union (minw = 19, nboot = 200) ─────────────────────────────────────
#> 
#>   series  supDF   supBZ      U  p_supDF  p_supBZ    p_U
#>     psy1  1.946  0.2802  1.946    0.055    0.675  0.105
#>     psy2  7.880  1.5349  7.880    0.000    0.175  0.000
#>    evans  5.283  1.9138  5.283    0.120    0.285  0.190
#>      div  1.113  2.2607  1.338    0.090    0.100  0.130
#>     blan  3.930  1.4008  3.930    0.080    0.240  0.130
#> 
autoplot(res)

# }
```
