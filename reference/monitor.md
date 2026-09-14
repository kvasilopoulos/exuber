# Real-Time Monitoring for Explosive Bubbles

`monitor` implements real-time monitoring: fix a training window
`[1, T*]` assumed free of exuberance, calibrate a critical value on it,
then compare the running recursive statistic at each subsequent point
`T*+1, ..., T` against that fixed boundary, flagging the first date it
is breached.

## Usage

``` r
monitor(
  data,
  r_star = 0.5,
  minw = NULL,
  nboot = 500L,
  sig_lvl = 95,
  lag = 0L,
  type = c("fixed", "aic", "bic"),
  seed = NULL,
  boundary = c("bootstrap", "kurozumi", "fluc"),
  s0 = 0
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

- r_star:

  The end of the training window: a fraction in `(0, 1)` of the sample
  (default `0.5`), or an integer observation count if `>= 1`.

- minw:

  A positive integer. The minimum window size (default = \\(0.01 +
  1.8/\sqrt{T})T\\, where T denotes the sample size).

- nboot:

  Number of wild bootstrap replications for the training critical value.
  Ignored unless `boundary = "bootstrap"`.

- sig_lvl:

  Significance level for the monitoring boundary on the package-wide
  0-100 scale, one of `90`, `95` (default), `99`.

- lag:

  A non-negative integer. The lag length of the Augmented Dickey-Fuller
  regression (default = 0L).

- type:

  Lag selection for the wild bootstrap DGP, passed to
  [`radf_wb_ps_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_ps_cv.md).
  Ignored unless `boundary = "bootstrap"`.

- seed:

  Optional seed for the bootstrap draws. Ignored unless
  `boundary = "bootstrap"`.

- boundary:

  `"bootstrap"` (default, Phillips & Shi 2020), `"kurozumi"` (Kurozumi
  2020's closed-form SADF/GSADF boundary), or `"fluc"` (Homm & Breitung
  2012's FLUC boundary).

- s0:

  Kurozumi (2020)'s window-start range as a fraction of the training
  length, only used when `boundary = "kurozumi"`. `0` (default) is the
  `SADF` case (window start fixed at `1`); `0.4` or `0.8` switches to
  the `GSADF_{s0}` case (window start ranges over
  `[1, floor(T* * s0)]`), the only two values his boundary function's
  scaling constants are tabulated for.

## Value

An object of class `monitor_obj`: a list with the full-sample statistic
path (`stat` – `bsadf` for `boundary = "bootstrap"`, `badf` for
`"kurozumi"`/ `"fluc"`), the calibrated `boundary` (one flat value per
series), the training window length `T_star`, and `alarm`/`alarm_date`
(the first monitoring-period observation/date at which `stat` breaches
the boundary, `NA` if never).

## Details

`boundary = "bootstrap"` (default) implements Phillips & Shi (2020): the
boundary is a wild-bootstrap quantile of the GSADF-type statistic
([`radf_wb_ps_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_ps_cv.md),
its `tb` parameter), compared against
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s
`bsadf` sequence. Deliberately calibrates on the training window *only*
(`data[1:T*]`), not the full series:
[`radf_wb_ps_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_ps_cv.md)'s
underlying null-model fit (`adf_res()`) uses whatever data it is given
in full, with no internal truncation to `tb` – passing post-`T*`
(possibly explosive) data to it directly would leak future information
into the null calibration.

`boundary = "kurozumi"` implements Kurozumi (2020)'s closed-form
alternative: no bootstrap at all, just a published constant (his
Table 1) compared against
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s
`badf` sequence (his `SADF(k)` detector – the `s0 = 0`, fixed-start-at-1
case, the default). Setting `s0` to `0.4` or `0.8` instead switches to
his `GSADF_{s0}(k)` generalization: the window start is allowed to range
over `[1, floor(T* * s0)]` rather than being fixed at `1`, compared
against his `k`-varying (not constant) boundary function and its own
published scaling constant. `sig_lvl` must be one of `90`, `95`, or `99`
(the levels his table tabulates).

`boundary = "fluc"` implements Homm & Breitung (2012)'s FLUC detector:
their `DF_{t/n}` is likewise exactly
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s
`badf` sequence, compared against a published constant from their Table
7 (no detrending case) rather than a simulated one. `sig_lvl` must be
one of `90`, `95`, `99`.

## Note

Returns its own class (not `radf_obj`), so it does not plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/`\link{datestamp}`/`tidy`;
it has its own [`print()`](https://rdrr.io/r/base/print.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods instead. Prints its own boundary/alarm summary (real-time
monitoring output, not a per-series sup-statistic table) – see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the full picture of which functions do and don't fit that pipeline.

## Status

**\[experimental\]**

## References

Phillips, P. C., & Shi, S. (2020). Real time monitoring of asset
markets: Bubbles and crises. In Handbook of Statistics (Vol. 42, pp.
61-80). Elsevier.

Kurozumi, E. (2020). Asymptotic properties of bubble monitoring tests.
Econometric Reviews, 39(5), 510-538.

Homm, U., & Breitung, J. (2012). Testing for speculative bubbles in
stock markets: A comparison of alternative methods. Journal of Financial
Econometrics, 10(1), 198-231.

## See also

[`radf_wb_ps_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_ps_cv.md)
for the underlying wild bootstrap, and
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
for the (non-monitoring, full-sample) origination/collapse dating that
already exists.

Other monitoring:
[`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md),
[`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md),
[`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md)

## Examples

``` r
# \donttest{
# A bubble-free training window (first half), explosive from t = 150 on
y <- sim_psy1(n = 200, te = 150, tf = 200, seed = 7)
# Default: Phillips & Shi (2020) wild bootstrap boundary
mon <- monitor(y, r_star = 0.5, nboot = 200)
print(mon)
#> 
#> ── monitor (T* = 100 / 200, minw = 27, sig_lvl = 95%, boundary = bootstrap) ────
#> 
#>    series  boundary  alarm  alarm_date
#>   series1     2.051    156         156
#> 
autoplot(mon)
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_segment()`).


# Kurozumi (2020) closed-form boundary -- no bootstrap needed
mon_kz <- monitor(y, r_star = 0.5, boundary = "kurozumi")
autoplot(mon_kz)
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_segment()`).


# Homm & Breitung (2012) FLUC boundary
autoplot(monitor(y, r_star = 0.5, boundary = "fluc"))
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_segment()`).

# }
```
