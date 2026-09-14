# Sequential LBI Monitoring for an Unknown Bubble Start Date (Breitung & Diegel 2025)

`monitor_lbi` implements the sequential (constant-boundary) extension of
[`lbi_test`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md)'s
locally best invariant statistic, for monitoring a series in real time
when the bubble's start date is unknown: after a training window
`[1, T*]` assumed free of exuberance, the (optionally exponentially
weighted) partial sum of post-training first differences is compared
against a constant boundary, flagging the first monitoring date it is
breached.

## Usage

``` r
monitor_lbi(data, r_star = 0.5, c_bar = 0, sig_lvl = 95)
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

- r_star:

  The end of the training window: a fraction in `(0, 1)` of the sample
  (default `0.5`), or an integer observation count if `>= 1`.

- c_bar:

  Exponential up-weighting parameter for later (more bubble-like)
  monitoring observations (their eq. 12), `>= 0`. `0` (default) is the
  flat-weight "mCUSUM" variant, appropriate when a bubble is equally
  likely to start at any point in the monitoring window; the paper's own
  suggested value for a moderate power boost when a bubble partway
  through is more plausible is `2`. Critical values (`sig_lvl`) are the
  same for every `c_bar`.

- sig_lvl:

  Significance level on the package-wide 0-100 scale, one of `90`, `95`,
  `97.5`, `99`, `99.5` (Breitung & Diegel's Table 1 only tabulates
  these).

## Value

An object of class `monitor_lbi_obj`: a list with the monitoring-region
statistic path (`stat`), the constant `boundary`, the training window
length `T_star`, and `alarm`/`alarm_date` (the first breach, `NA` if
none).

## Details

Their eq. 15 shows this partial sum, normalized by the fixed monitoring
horizon length (not `sqrt(t)`, unlike
[`monitor_cusum`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md)'s
Chu-Stinchcombe-White-style boundary), converges to a standard Brownian
motion on `[0, 1]` under the null – so a single constant boundary
controls size uniformly across the whole monitoring window. The paper
shows this constant-boundary detector ("mCUSUM" at `c_bar = 0`, "wCUSUM"
at `c_bar > 0`) is more powerful than the classical
time-varying-boundary CUSUM test it is compared against.

## Note

The critical value is a published constant boundary (Breitung & Diegel
(2025)'s Table 1) – a table lookup, no simulation.

Returns its own class (not `radf_obj`), so it does not plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/`\link{datestamp}`/`tidy`;
it has its own [`print()`](https://rdrr.io/r/base/print.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods instead. Prints its own boundary/alarm summary – see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the full picture of which functions do and don't fit that pipeline.

## Status

**\[experimental\]**

## References

Breitung, J., & Diegel, M. (2025). A locally best invariant sequential
test for explosive behavior in the presence of nonstationary volatility.
Journal of Time Series Analysis.

## See also

[`lbi_test`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md)
for the static (known, full-sample bubble window) version.
[`monitor_cusum`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md)
and
[`monitor`](https://kvasilopoulos.github.io/exuber/reference/monitor.md)
for structurally different monitoring detectors.

Other monitoring:
[`monitor()`](https://kvasilopoulos.github.io/exuber/reference/monitor.md),
[`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md),
[`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md)

## Examples

``` r
# \donttest{
# A martingale training window, explosive from t = 150 to the sample end
y <- sim_psy1(n = 200, te = 150, tf = 200, seed = 7)
res <- monitor_lbi(y, r_star = 100)
print(res) # alarm should fire soon after t = 150
#> 
#> ── monitor_lbi (T* = 100 / 200, c_bar = 0, b_alpha = 1.95) ─────────────────────
#> 
#>    series  alarm  alarm_date
#>   series1    155         155
#> 
autoplot(res)
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_segment()`).


# wCUSUM: exponentially up-weight later monitoring observations
autoplot(monitor_lbi(y, r_star = 100, c_bar = 2))
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_segment()`).

# }
```
