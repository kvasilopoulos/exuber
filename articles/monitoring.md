# Real-Time Monitoring

``` r

library(exuber)
```

## Monitoring vs. testing

[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)/[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
and the `dating_*()` family (see
[`vignette("dating-methods")`](https://kvasilopoulos.github.io/exuber/articles/dating-methods.md))
all work on a *finished* sample: they answer “was there a bubble, and
when” after every observation is already in hand. The `monitor_*()`
family answers a different, real-time question: fix a training window
`[1, T*]` believed free of exuberance, calibrate a boundary on it, then
watch each new observation `T*+1, T*+2, ...` and raise an alarm the
first time the boundary is breached. All four functions share this
`r_star`/alarm/`alarm_date` shape; they differ in what statistic they
monitor and how the boundary is calibrated.

| Function | Statistic monitored | Boundary |
|----|----|----|
| [`monitor_radf()`](https://kvasilopoulos.github.io/exuber/reference/monitor_radf.md) | [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)’s own `badf`/`bsadf` recursion | `"bootstrap"` (Phillips & Shi 2020 wild-bootstrap quantile), `"kurozumi"` (closed-form, Kurozumi 2020), or `"fluc"` (closed-form, Homm & Breitung 2012) |
| [`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md) | A CUSUM of the training-window-standardized series | Homm & Breitung (2012)’s asymptotic (or finite-sample) constant |
| [`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md) | Breitung & Diegel (2025)’s locally-best-invariant CUSUM (`mCUSUM`/`wCUSUM`, via `c_bar`) | Their Table 1 constant |
| [`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md) | A recursive quantile regression at `tau` | A simulated first-crossing boundary (Wu, Shi & Wu 2025) |

[`monitor_radf()`](https://kvasilopoulos.github.io/exuber/reference/monitor_radf.md)
keeps the `radf_` internals (it reuses `badf`/`bsadf` directly) but is
named for what it *does* – see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for why the naming convention crosses that line deliberately.

## The same bubble, five monitors

A training window of pure random walk (`T* = 100`), followed by more
random walk, then a genuine explosive regime (`rho = 1.04`) starting at
`t = 150`:

``` r

make_bubble_series <- function(n, T_star, bstart, rho = 1.04) {
  y <- numeric(n)
  y[seq_len(T_star)] <- cumsum(rnorm(T_star))
  for (t in (T_star + 1):n) {
    y[t] <- if (t < bstart) y[t - 1] + rnorm(1) else rho * y[t - 1] + rnorm(1)
  }
  y
}
set.seed(7)
y <- make_bubble_series(200, T_star = 100, bstart = 150)
```

``` r

monitor_lbi(y, r_star = 100)
#> 
#> ── monitor_lbi (T* = 100 / 200, c_bar = 0, b_alpha = 1.95) ─────────────────────
#> 
#>    series  alarm  alarm_date
#>   series1    159         159
monitor_cusum(y, r_star = 0.5)
#> 
#> ── monitor_cusum (T* = 100 / 200, b_alpha = 4.6) ───────────────────────────────
#> 
#>    series  alarm  alarm_date
#>   series1    164         164
monitor_quantile(y, tau = 0.5, nrep = 200, seed = 1)
#> 
#> ── monitor_quantile (n = 200, minw = 27, tau = 0.5, level = 95%) ───────────────
#> 
#>    series  delta  boundary  alarm  alarm_date
#>   series1  0.695     1.757    163         163
monitor_radf(y, r_star = 0.5, nboot = 200, seed = 1)
#> 
#> ── monitor_radf (T* = 100 / 200, minw = 27, level = 95%, boundary = bootstrap) ─
#> 
#>    series  boundary  alarm  alarm_date
#>   series1     2.025    160         160
monitor_radf(y, r_star = 0.5, boundary = "kurozumi")
#> 
#> ── monitor_radf (T* = 100 / 200, minw = 27, level = 95%, boundary = kurozumi) ──
#> 
#>    series  boundary  alarm  alarm_date
#>   series1     1.038    161         161
```

Every monitor here alarms within about 15 points of the true bubble
start (150), none before it – that “never before `T*` (or the true
start)” property is exactly what each function’s own test suite checks
under the null. Alarm *timing* differs by design:
[`monitor_radf()`](https://kvasilopoulos.github.io/exuber/reference/monitor_radf.md)’s
ADF-family statistics tend to detect mid-sample bubbles fastest (the
literature’s own finding, e.g. Kurozumi 2020/2021), while CUSUM-type
detectors
([`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md),
[`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md))
are typically slower but computationally simpler and don’t need a
bootstrap.

## Which to reach for

- Fastest detection, willing to pay for a wild bootstrap per call:
  `monitor_radf(boundary = "bootstrap")` (the default).
- Same statistic, no bootstrap, an off-the-shelf published constant
  instead: `monitor_radf(boundary = "kurozumi")` or `boundary = "fluc"`.
- A simpler CUSUM-based alternative with its own closed-form boundary:
  [`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md),
  or
  [`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md)
  for Breitung & Diegel’s locally-best-invariant version (`c_bar > 0`
  trades a little size for power against slow-building bubbles).
- Monitoring a specific quantile of the distribution rather than the
  mean behavior:
  [`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md).
