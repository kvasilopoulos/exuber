# Real-Time Monitoring

``` r

library(exuber)
```

## Monitoring and testing

[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md),
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
and the `dating_*()` family (see
[`vignette("dating-methods")`](https://kvasilopoulos.github.io/exuber/articles/dating-methods.md))
all work on a finished sample. They answer the question of whether there
was a bubble, and when, after every observation is already in hand. The
`monitor_*()` family answers a real-time question instead. We fix a
training window `[1, T*]` that we believe is free of exuberance and
calibrate a boundary on it. We then watch each new observation
`T*+1, T*+2, ...` and raise an alarm the first time the boundary is
crossed. All four functions share this `r_star`, alarm and `alarm_date`
structure. They differ in the statistic they monitor and in how they
calibrate the boundary.

| Function | Statistic monitored | Boundary | Static, full-sample counterpart |
|----|----|----|----|
| [`monitor()`](https://kvasilopoulos.github.io/exuber/reference/monitor.md) | [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)’s own `badf`/`bsadf` recursion | `"bootstrap"` (Phillips & Shi 2020 wild-bootstrap quantile), `"kurozumi"` (closed-form, Kurozumi 2020), or `"fluc"` (closed-form, Homm & Breitung 2012) | [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md) and [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md). It is not a `_test()` function, but it uses the same recursive ADF core |
| [`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md) | A CUSUM of the training-window-standardized series | Homm & Breitung (2012)’s asymptotic (or finite-sample) constant | None. The CUSUM boundary of Homm & Breitung is a training and monitoring construction by design and has no full-sample form |
| [`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md) | Breitung & Diegel (2025)’s locally-best-invariant CUSUM (`mCUSUM`/`wCUSUM`, via `c_bar`) | Their Table 1 constant | [`lbi_test()`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md), the static version of the same statistic |
| [`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md) | A recursive quantile regression at `tau` | A simulated first-crossing boundary (Wu, Shi & Wu 2025) | [`quantile_test()`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md), the static version of the same statistic |

[`monitor()`](https://kvasilopoulos.github.io/exuber/reference/monitor.md)
reuses `badf` and `bsadf` directly, which is the same recursive ADF core
as in the `radf_*()` family. It is named after what it does and not
after that internal detail (see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the reasoning). Two of its three siblings,
[`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md)
and
[`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md),
are the sequential extension of an existing static test with the same
name minus the `monitor_` prefix.
[`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md)
has no such counterpart, because its source paper (Homm & Breitung 2012)
proposed the CUSUM only as a monitoring detector.

## One bubble, five monitors

The series starts with a training window of pure random walk
(`T* = 100`) and continues as a random walk until `t = 150`. From then
until the end of the sample it follows an explosive regime
(`rho = 1.04`). We generate it with
[`sim_psy1()`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md),
placing the bubble after the training window and leaving out the
collapse:

``` r

y <- sim_psy1(n = 200, te = 150, tf = 200, c = 0.04, alpha = 0, seed = 7)
```

``` r

monitor_lbi(y, r_star = 100)
#> 
#> ── monitor_lbi (T* = 100 / 200, c_bar = 0, b_alpha = 1.95) ─────────────────────
#> 
#>    series  alarm  alarm_date
#>   series1    156         156
monitor_cusum(y, r_star = 0.5)
#> 
#> ── monitor_cusum (T* = 100 / 200, b_alpha = 4.6) ───────────────────────────────
#> 
#>    series  alarm  alarm_date
#>   series1    161         161
monitor_quantile(y, tau = 0.5, nrep = 200, seed = 1)
#> 
#> ── monitor_quantile (QPWY, n = 200, minw = 27, tau = 0.5, sig_lvl = 95%) ───────
#> 
#>    series  delta  boundary  alarm  alarm_date
#>   series1   0.64     1.909    161         161
monitor(y, r_star = 0.5, nboot = 200, seed = 1)
#> 
#> ── monitor (T* = 100 / 200, minw = 27, sig_lvl = 95%, boundary = bootstrap) ────
#> 
#>    series  boundary  alarm  alarm_date
#>   series1      2.17    156         156
monitor(y, r_star = 0.5, boundary = "kurozumi")
#> 
#> ── monitor (T* = 100 / 200, minw = 27, sig_lvl = 95%, boundary = kurozumi) ─────
#> 
#>    series  boundary  alarm  alarm_date
#>   series1     1.038    159         159
```

[`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md)
and
[`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md)
each have a static, full-sample counterpart that asks the retrospective
version of the same question. It runs on the whole series and does not
wait for a first crossing:

``` r

lbi_test(y)
#> 
#> ── lbi_test (n = 200, sig_lvl = 95%) ───────────────────────────────────────────
#> 
#>    series   stat   crit  detected
#>   series1  6.502  1.645      TRUE
quantile_test(y, tau = 0.5)
#> 
#> ── quantile_test (n = 200, sig_lvl = 95%) ──────────────────────────────────────
#> 
#>    series  tau  tstat    crit  delta  detected
#>   series1  0.5  20.25  0.5041   0.64      TRUE
```

Every monitor alarms within about 15 observations of the true bubble
start (150), and none alarms before it. Each function’s own test suite
checks the absence of alarms before `T*` (or the true start) under the
null. The timing of the alarm differs by design. The ADF-family
statistics in
[`monitor()`](https://kvasilopoulos.github.io/exuber/reference/monitor.md)
tend to detect bubbles in the middle of the sample fastest, as the
literature finds (for example Kurozumi 2020, 2021). The CUSUM-type
detectors
([`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md)
and
[`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md))
are typically slower, but they are computationally simpler and need no
bootstrap.

## Which to reach for

- If you want the fastest detection and can afford a wild bootstrap on
  each call, use `monitor(boundary = "bootstrap")`, which is the
  default.
- If you want the same statistic without a bootstrap, using a published
  constant instead, use `monitor(boundary = "kurozumi")` or
  `monitor(boundary = "fluc")`.
- If you want a simpler CUSUM-based alternative with its own closed-form
  boundary, use
  [`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md).
  For the locally best invariant version of Breitung & Diegel, use
  [`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md),
  where `c_bar > 0` gives up a little size in exchange for power against
  slowly building bubbles.
- If you want to monitor a specific quantile of the distribution instead
  of the mean, use
  [`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md).
