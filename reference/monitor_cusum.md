# CUSUM Real-Time Monitoring for Explosive Bubbles

`monitor_cusum` implements Homm & Breitung (2012)'s CUSUM real-time
monitoring procedure: fix a training window `[1, T*]` assumed free of
exuberance, then compare the standardized cumulative sum of
post-training first differences, `S_t = (y_t - y_{T*}) / sigma_hat_t`,
against a closed-form boundary `c_t * sqrt(t)`,
`c_t = sqrt(b_alpha + log(t / T*))`, flagging the first date it is
breached.

## Usage

``` r
monitor_cusum(
  data,
  r_star = 0.5,
  b_alpha = 4.6,
  boundary = c("asymptotic", "finite"),
  level = 0.95,
  type = c("standard", "kernel"),
  N = 20,
  kernel = c("gaussian", "uniform")
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

- b_alpha:

  The boundary constant (HB's eq. 29). Default `4.6`, HB's own one-sided
  asymptotic calibration for a 5\\ (their Section 3); this is an
  asymptotic upper bound on the false- alarm probability (Chu,
  Stinchcombe & White 1996), not an exact size, so it is typically
  conservative in finite samples. Ignored when `boundary = "finite"`.

- boundary:

  `"asymptotic"` (default) uses `b_alpha` directly. `"finite"` instead
  looks up HB's own finite-sample boundary constant (their Table 8) from
  `level` and the realized training length/monitoring-horizon ratio –
  `level` must then be one of `0.90`, `0.95`, `0.99`.

- level:

  Nominal confidence level when `boundary = "finite"` (default `0.95`);
  ignored when `boundary = "asymptotic"`.

- type:

  `"standard"` (default) for Homm & Breitung (2012)'s original CUSUM
  statistic, or `"kernel"` for Astill, Harvey, Leybourne, Taylor & Zu
  (2023)'s volatility-robust "CUSUMV" variant.

- N:

  Bandwidth/window length for the one-sided kernel spot-variance
  estimator when `type = "kernel"`. Default `20`, the authors' own
  empirically-recommended value (their Section 3: "setting H = 20
  delivered a procedure with the best trade-off" between false-alarm
  robustness and power). Ignored when `type = "standard"`.

- kernel:

  Kernel for the spot-variance estimator when `type = "kernel"`,
  `"gaussian"` (default) or `"uniform"`. Ignored when
  `type = "standard"`.

## Value

An object of class `monitor_cusum_obj`: a list with the
monitoring-region statistic path (`S`) and `boundary`, the training
window length `T_star`, and `alarm`/`alarm_date` (the first breach, `NA`
if none).

## Note

The boundary is closed-form throughout: a fixed asymptotic constant
(`boundary = "asymptotic"`, `b_alpha = 4.6`) or a published
finite-sample table lookup (`boundary = "finite"`, Homm & Breitung
(2012)'s Table 8) – no simulation, no separate cv function.

Unlike
[`monitor_radf`](https://kvasilopoulos.github.io/exuber/reference/monitor_radf.md)
(Family A, a recursive ADF-family statistic requiring a wild bootstrap
to calibrate its boundary), this is a structurally different statistic –
a standardized running sum, not a recursive regression – with an
asymptotic closed-form boundary (Chu, Stinchcombe & White 1996's
inequality, HB's eq. 28): no bootstrap, no simulation, no dependence on
the data beyond the running variance estimate itself.

`type = "kernel"` instead uses Astill, Harvey, Leybourne, Taylor & Zu
(2023)'s volatility-robust modification ("CUSUMV"): each first
difference is standardized by its own one-sided kernel spot-variance
estimate (their eq. 6-7) instead of a single running variance, before
cumulating. Their Corollary 1 establishes the *same* boundary function
delivers a controlled asymptotic false-alarm rate even under
time-varying volatility, unlike the standard CUSUM statistic, which
requires homoskedasticity for its own size-control result to hold.

Returns its own class (not `radf_obj`), so it does not plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/`\link{datestamp}`/`tidy`/`autoplot`
– prints its own boundary/alarm summary – see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the full picture of which functions do and don't fit that pipeline.

## Status

**\[experimental\]**

## References

Homm, U., & Breitung, J. (2012). Testing for speculative bubbles in
stock markets: A comparison of alternative methods. Journal of Financial
Econometrics, 10(1), 198-231.

Chu, C. S. J., Stinchcombe, M., & White, H. (1996). Monitoring
structural change. Econometrica, 64(5), 1045-1065.

Astill, S., Harvey, D. I., Leybourne, S. J., Taylor, A. M. R., & Zu, Y.
(2023). CUSUM-based monitoring for explosive episodes in financial data
in the presence of time-varying volatility. Journal of Financial
Econometrics, 21(1), 187-227.

## See also

[`monitor_radf`](https://kvasilopoulos.github.io/exuber/reference/monitor_radf.md)
for the recursive-ADF (Family A) monitoring alternative.

## Examples

``` r
# \donttest{
make_bubble_series <- function(n, T_star, bstart, rho = 1.04) {
  y <- numeric(n)
  y[seq_len(T_star)] <- cumsum(rnorm(T_star))
  for (t in (T_star + 1):n) {
    y[t] <- if (t < bstart) y[t - 1] + rnorm(1) else rho * y[t - 1] + rnorm(1)
  }
  y
}
set.seed(7)
y <- make_bubble_series(200, T_star = 100, bstart = 150) # bubble starts at 150
res <- monitor_cusum(y, r_star = 0.5)
print(res) # alarm should fire soon after t = 150
#> 
#> ── monitor_cusum (T* = 100 / 200, b_alpha = 4.6) ───────────────────────────────
#> 
#>    series  alarm  alarm_date
#>   series1    164         164
#> 
# }
```
