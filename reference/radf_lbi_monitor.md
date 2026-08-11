# Sequential LBI Monitoring for an Unknown Bubble Start Date (Breitung & Diegel 2025)

`radf_lbi_monitor` implements the sequential (constant-boundary)
extension of
[`radf_lbi`](https://kvasilopoulos.github.io/exuber/reference/radf_lbi.md)'s
locally best invariant statistic, for monitoring a series in real time
when the bubble's start date is unknown: after a training window
`[1, T*]` assumed free of exuberance, the (optionally exponentially
weighted) partial sum of post-training first differences is compared
against a constant boundary, flagging the first monitoring date it is
breached.

## Usage

``` r
radf_lbi_monitor(data, r_star = 0.5, c_bar = 0, level = 0.95)
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
  through is more plausible is `2`. Critical values (`level`) are the
  same for every `c_bar`.

- level:

  Nominal confidence level, one of `0.90`, `0.95`, `0.975`, `0.99`,
  `0.995` (Breitung & Diegel's Table 1 only tabulates these).

## Value

An object of class `radf_lbi_monitor_obj`: a list with the
monitoring-region statistic path (`stat`), the constant `boundary`, the
training window length `T_star`, and `alarm`/`alarm_date` (the first
breach, `NA` if none).

## Details

Their eq. 15 shows this partial sum, normalized by the fixed monitoring
horizon length (not `sqrt(t)`, unlike
[`radf_cusum`](https://kvasilopoulos.github.io/exuber/reference/radf_cusum.md)'s
Chu-Stinchcombe-White-style boundary), converges to a standard Brownian
motion on `[0, 1]` under the null – so a single constant boundary
controls size uniformly across the whole monitoring window. The paper
shows this constant-boundary detector ("mCUSUM" at `c_bar = 0`, "wCUSUM"
at `c_bar > 0`) is more powerful than the classical
time-varying-boundary CUSUM test it is compared against.

## Status

**\[experimental\]**

## References

Breitung, J., & Diegel, M. (2025). A locally best invariant sequential
test for explosive behavior in the presence of nonstationary volatility.
Journal of Time Series Analysis.

## See also

[`radf_lbi`](https://kvasilopoulos.github.io/exuber/reference/radf_lbi.md)
for the static (known, full-sample bubble window) version.
[`radf_cusum`](https://kvasilopoulos.github.io/exuber/reference/radf_cusum.md)
and
[`radf_monitor`](https://kvasilopoulos.github.io/exuber/reference/radf_monitor.md)
for structurally different monitoring detectors.
