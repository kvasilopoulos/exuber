# QPWY/QPSY Recursive Quantile Monitoring (Wu, Shi & Wu 2025)

`monitor_quantile` implements the QPWY and QPSY real-time monitoring
strategies of Wu, Shi & Wu (2025): quantile-regression (QR) analogues of
PWY's and PSY's recursive ADF t-statistics, testing at a chosen
conditional quantile `tau` rather than
[`quantile_test`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)'s
single full-sample test. `type = "qpwy"` uses the expanding window
`[1, r]`
([`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s
own `badf` shape); `type = "qpsy"` takes the supremum over every window
start as well (`bsadf`'s shape).

## Usage

``` r
monitor_quantile(
  data,
  tau = 0.5,
  minw = NULL,
  nrep = 500L,
  sig_lvl = 95,
  seed = NULL,
  type = c("qpwy", "qpsy")
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
  1.8/\sqrt{T})T\\, where T denotes the sample size).

- nrep:

  Number of Monte Carlo replications for the boundary.

- sig_lvl:

  Significance level, one of `90`, `95`, `99`.

- seed:

  Optional seed for the Monte Carlo draws.

- type:

  `"qpwy"` (expanding window) or `"qpsy"` (supremum over window starts
  too).

## Value

An object of class `monitor_quantile_obj`: a list with the statistic
path `stat`, the (flat) `boundary`, the estimated `delta`, and
`alarm`/`alarm_date` (the first breach, `NA` if none).

## Details

The point statistic needs genuine QR fits (no closed-form recursive
update the way OLS has): `O(T)` of them for QPWY, `O(T^2)` for QPSY, so
QPSY takes seconds per series at `n = 200` and grows quadratically.

The critical value is simulated per call from the limiting null
distribution `delta * Q_{r1,r2} + sqrt(1 - delta^2) * Z_{r1,r2}`, with
`delta` a data-estimated correlation coefficient (as in
[`quantile_test`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)),
`Q` the Dickey-Fuller t functional and `Z` its counterpart driven by an
independent Brownian motion, both simulated for every window (no QR fits
needed). A single **flat** boundary is used (not one value per `r`): the
quantile of each simulated path's own supremum, exactly how
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)'s
own `sadf_cv` is constructed, which controls the first-crossing
false-alarm rate.

## Note

The critical value (boundary) is simulated internally on every call (via
an unexported helper, `quantile_boundary_sim`) – there is currently no
reusable/exported cv counterpart for this function (a known,
separately-tracked gap, not addressed here).

Returns its own class (not `radf_obj`), so it does not plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/`\link{datestamp}`/`tidy`;
it has its own [`print()`](https://rdrr.io/r/base/print.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods instead. Prints its own statistic/boundary/delta summary – see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the full picture of which functions do and don't fit that pipeline.

## Caveats

The boundary is the asymptotic one. Near the median it is well sized in
finite samples (false-alarm rate 3.5-4.0\\ \\t_3\\ innovations,
`tau = 0.5`). Away from the median the small early windows make both
statistics oversized, QPSY badly so, even with Gaussian innovations:
QPSY's false-alarm rate is 35\\ (Gaussian) and, with \\t_3\\
innovations, 21\\ 44\\ innovations, is 7.5-8.5\\ `tau = 0.9`
(`n = 150`). Wu, Shi & Wu advise against extreme quantiles in small
samples and use bootstrap critical values for monitoring; that bootstrap
is not implemented here. For `type = "qpsy"` with `tau` away from 0.5 a
short pointer is emitted as a message (see
[`suppressMessages`](https://rdrr.io/r/base/message.html)) and stored as
`attr(x, "caveat")`. Numbers: docs/alternative-paradigms.md.

## Status

**\[experimental\]**

## References

Wu, R., Shi, S., & Wu, J. (2025). Quantile analysis for financial bubble
detection and surveillance. Journal of Time Series Analysis, 46(5),
908-931.

## See also

[`quantile_test`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)
for the static, full-sample version of this test.
[`monitor`](https://kvasilopoulos.github.io/exuber/reference/monitor.md)
for the OLS-based monitoring alternative.

Other monitoring:
[`monitor()`](https://kvasilopoulos.github.io/exuber/reference/monitor.md),
[`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md),
[`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md)

## Examples

``` r
# \donttest{
# Heavy-tailed (t3) innovations, explosive from t = 150 to the sample end
y <- sim_psy1(n = 200, te = 150, tf = 200, seed = 7,
  e = sim_innov(199, dist = "t", df = 3))
res <- monitor_quantile(y, tau = 0.5, nrep = 100, seed = 1)
print(res)
#> 
#> ── monitor_quantile (QPWY, n = 200, minw = 27, tau = 0.5, sig_lvl = 95%) ───────
#> 
#>    series  delta  boundary  alarm  alarm_date
#>   series1  0.623     1.934    165         165
#> 
autoplot(res)
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_segment()`).


# Upper-quantile monitoring is typically more powerful for right-tailed
# bubbles, but see the Caveats section on extreme quantiles
autoplot(monitor_quantile(y, tau = 0.8, nrep = 100, seed = 1))
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_segment()`).


# QPSY: supremum over window starts too (O(n^2) QR fits, slower)
monitor_quantile(y[101:200], tau = 0.5, nrep = 100, seed = 1, type = "qpsy")
#> 
#> ── monitor_quantile (QPSY, n = 100, minw = 19, tau = 0.5, sig_lvl = 95%) ───────
#> 
#>    series  delta  boundary  alarm  alarm_date
#>   series1  0.754     2.172     53          53
#> 
# }
```
