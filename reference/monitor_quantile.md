# QPWY Recursive Quantile Monitoring (Wu, Shi & Wu 2025)

`monitor_quantile` implements the QPWY real-time monitoring strategy of
Wu, Shi & Wu (2025): a quantile-regression (QR) analogue of PWY's own
recursive ADF t-statistic, testing at a chosen conditional quantile
`tau` over an expanding window `[1, r]` (start fixed at the beginning of
the sample, exactly
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s own
`badf` convention) rather than
[`quantile_test`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)'s
single full-sample test.

## Usage

``` r
monitor_quantile(
  data,
  tau = 0.5,
  minw = NULL,
  nrep = 500L,
  level = 95,
  seed = NULL
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

- tau:

  Quantile to test at, in `(0, 1)` (fixed, unlike
  [`quantile_test`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)'s
  `"optimal"` grid search – WSW's own eq. 25 takes `tau` as a given
  parameter for the monitoring statistic, not re-selected at each
  recursion point).

- minw:

  A positive integer. The minimum window size (default = \\(0.01 +
  1.8/\sqrt(T))T\\, where T denotes the sample size).

- nrep:

  Number of Monte Carlo replications for the boundary.

- level:

  Significance level, one of `90`, `95`, `99`.

- seed:

  Optional seed for the Monte Carlo draws.

## Value

An object of class `monitor_quantile_obj`: a list with the statistic
path `stat`, the (flat) `boundary`, the estimated `delta`, and
`alarm`/`alarm_date` (the first breach, `NA` if none).

## Details

Only `QPWY` (single recursion) is implemented, not the paper's own
`QPSY` (double recursion, additionally optimizing over the window
start): `QPWY_r(tau)` needs `O(T)` actual quantile -regression fits (no
closed-form recursive update the way OLS has), tractable at the same
cost order as
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s
own `badf`; `QPSY` needs `O(T^2)` such fits, a substantially larger
undertaking left unimplemented.

The critical value is simulated per call: `QPWY_r(tau)`'s limiting null
distribution at each `r` is `sqrt(1 - delta^2) * z + delta * Q_{0,r}`,
with `z ~ N(0, 1)`, `delta` a data-estimated correlation coefficient (as
in
[`quantile_test`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)),
and `Q_{0,r}` exactly
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s
own `badf` sequence under a simulated null path – reusing
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
directly for the simulation rather than new theory. A single **flat**
boundary is used (not one value per `r`): controlling the first-crossing
false-alarm rate requires calibrating against each simulated path's own
supremum, exactly how
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)'s
own `sadf_cv` is constructed, not a per-`r` marginal quantile (which
would badly inflate the false-alarm rate).

## Note

The critical value (boundary) is simulated internally on every call (via
an unexported helper, `qpwy_boundary_sim`) – there is currently no
reusable/exported cv counterpart for this function (a known,
separately-tracked gap, not addressed here).

Returns its own class (not `radf_obj`), so it does not plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/`\link{datestamp}`/`tidy`/`autoplot`
– prints its own statistic/boundary/delta summary – see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the full picture of which functions do and don't fit that pipeline.

## References

Wu, R., Shi, S., & Wu, J. (2025). Quantile analysis for financial bubble
detection and surveillance. Journal of Time Series Analysis, 46(5),
908-931.

## See also

[`quantile_test`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)
for the static, full-sample version of this test.
[`monitor_radf`](https://kvasilopoulos.github.io/exuber/reference/monitor_radf.md)
for the OLS-based monitoring alternative.

## Examples

``` r
# \donttest{
res <- monitor_quantile(sim_data$psy2, tau = 0.5, nrep = 100, seed = 1)
print(res)
#> 
#> ── monitor_quantile (n = 100, minw = 19, tau = 0.5, level = 95%) ───────────────
#> 
#>    series  delta  boundary  alarm  alarm_date
#>   series1  0.331     1.299     24          24
#> 
# }
```
