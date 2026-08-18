# Sequential Sample-Splitting Bubble Dating (PDC/KS)

`dating_pdc` dates a single bubble episode using the sequential
sample-splitting method of Pang, Du & Chong (2021) and its 4-regime
extension by Kurozumi & Skrobotov (2023): a fixed regime structure
(unit-root, explosive, stationary-collapse, and optionally a final
unit-root recovery regime) whose breakpoints are estimated one at a
time, each a closed-form residual-sum-of-squares minimisation over a
no-intercept AR(1) model, in \\O(T)\\ via cumulative sums.

## Usage

``` r
dating_pdc(
  data,
  regimes = 3L,
  trim = 0.05,
  type = c("ols", "wls"),
  kernel = c("gaussian", "uniform"),
  h = NULL
)
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

- regimes:

  Either `3` (PDC: unit-root, explosive, stationary collapse) or `4`
  (KS: adds a final unit-root recovery regime after the collapse).

- trim:

  Minimum fraction of the (differenced) sample required on either side
  of each breakpoint search (default 0.05, as in KS's empirical
  application; PDC use 0.05-0.1 in their simulations).

- type:

  `"ols"` (default) for the plain homoskedastic estimator, or `"wls"`
  for Kurozumi & Skrobotov (2023)'s volatility-corrected two-step
  estimator.

- kernel:

  Kernel for the spot-volatility estimator when `type = "wls"`,
  `"gaussian"` (default) or `"uniform"`. Ignored when `type = "ols"`.

- h:

  Bandwidth for the spot-volatility estimator when `type = "wls"`.
  Default: leave-one-out cross-validation. Ignored when `type = "ols"`.

## Value

A `data.frame` with one row per series and columns `origination`,
`collapse`, and (if `regimes = 4`) `recovery`, giving the estimated
break dates (or observation indices, if no date index is available).

## Details

Unlike
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
(which finds where the recursive BSADF statistic crosses a critical
value), this fits an explicit regime-switching model directly to the
series; it needs no critical values at all. PDC prove the collapse date
is identified first – its effect on the residual sum of squares
dominates the origination date's – which is what licenses estimating the
breaks sequentially rather than jointly (unlike Harvey, Leybourne &
Sollis's (2017) BIC-selected, jointly-fit alternative, which is not
implemented here; see the package's enhancement notes for the
cost/benefit reasoning).

`type = "wls"` adds Kurozumi & Skrobotov (2023)'s time-varying-
volatility correction: fit the plain (`"ols"`) model first, collect its
fitted piecewise-regime residuals, smooth their square nonparametrically
(the same Nadaraya-Watson kernel/leave-one-out bandwidth estimator
exuber already uses for
[`radf_sbz_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md)/[`radf_kp`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md)),
and re-run the same sequential break search with each squared term
weighted by the inverse of the estimated spot variance. This needs no
new critical-value theory – like the OLS version, it is point
estimation, not a threshold-crossing test.

## Note

Returns its own class (not `radf_obj`), so it does not plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/`\link{datestamp}`/`tidy`/`autoplot`
– prints its own dating table (model, origination, collapse, recovery) –
see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the full picture of which functions do and don't fit that pipeline.

## Status

**\[experimental\]**

## References

Pang, T., Du, L., & Chong, T. T. L. (2021). Estimating multiple breaks
in the bubble regime with SSR minimization. Journal of Management
Science and Engineering.

Kurozumi, E., & Skrobotov, A. (2023). Bubble dating: a sequential
testing approach.

Kurozumi, E., & Skrobotov, A. (2023). Improving the accuracy of bubble
date estimators under time-varying volatility. arXiv:2306.02977.

## See also

[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
for the PSY threshold-crossing alternative.

## Examples

``` r
# \donttest{
res <- dating_pdc(sim_data$psy1, regimes = 3L, trim = 0.05)
print(res)
#>         origination collapse
#> series1          40       54
# }
```
