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

| Function | Statistic monitored | Boundary | Static, full-sample counterpart |
|----|----|----|----|
| [`monitor()`](https://kvasilopoulos.github.io/exuber/reference/monitor.md) | [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)’s own `badf`/`bsadf` recursion | `"bootstrap"` (Phillips & Shi 2020 wild-bootstrap quantile), `"kurozumi"` (closed-form, Kurozumi 2020), or `"fluc"` (closed-form, Homm & Breitung 2012) | [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)/[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md) – not a `_test()`, but the same recursive-ADF core |
| [`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md) | A CUSUM of the training-window-standardized series | Homm & Breitung (2012)’s asymptotic (or finite-sample) constant | none – Homm & Breitung’s CUSUM boundary is inherently a training/monitoring construction, with no full-sample form |
| [`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md) | Breitung & Diegel (2025)’s locally-best-invariant CUSUM (`mCUSUM`/`wCUSUM`, via `c_bar`) | Their Table 1 constant | [`lbi_test()`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md), the static version of the same statistic |
| [`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md) | A recursive quantile regression at `tau` | A simulated first-crossing boundary (Wu, Shi & Wu 2025) | [`quantile_test()`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md), the static version of the same statistic |

[`monitor()`](https://kvasilopoulos.github.io/exuber/reference/monitor.md)
reuses `badf`/`bsadf` directly (the same recursive-ADF core as the
`radf_*()` family) but is named for what it *does*, not that internal
detail – see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for why. Two of its three siblings
([`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md),
[`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md))
are the sequential extension of an existing static test of the same name
minus the `monitor_` prefix;
[`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md)
has no such counterpart, since its source paper (Homm & Breitung 2012)
proposed CUSUM as a monitoring detector only.

## The same bubble, five monitors

A training window of pure random walk (`T* = 100`), followed by more
random walk, then a genuine explosive regime (`rho = 1.04`) from
`t = 150` to the end of the sample –
[`sim_psy1()`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)
with the bubble pushed past the training window and no collapse:

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
#> ── monitor_quantile (n = 200, minw = 27, tau = 0.5, level = 95%) ───────────────
#> 
#>    series  delta  boundary  alarm  alarm_date
#>   series1   0.64     1.867    161         161
monitor(y, r_star = 0.5, nboot = 200, seed = 1)
#> 
#> ── monitor (T* = 100 / 200, minw = 27, level = 95%, boundary = bootstrap) ──────
#> 
#>    series  boundary  alarm  alarm_date
#>   series1      2.17    156         156
monitor(y, r_star = 0.5, boundary = "kurozumi")
#> 
#> ── monitor (T* = 100 / 200, minw = 27, level = 95%, boundary = kurozumi) ───────
#> 
#>    series  boundary  alarm  alarm_date
#>   series1     1.038    159         159
```

[`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md)
and
[`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md)
each have a static, full-sample sibling that asks the retrospective
version of the same question – run on the whole series rather than
watching for a first crossing:

``` r

lbi_test(y)
#> 
#> ── lbi_test (n = 200, level = 95%) ─────────────────────────────────────────────
#> 
#>    series   stat   crit  detected
#>   series1  6.502  1.645      TRUE
quantile_test(y, tau = 0.5)
#> 
#> ── quantile_test (n = 200, level = 95%) ────────────────────────────────────────
#> 
#>    series  tau  tstat    crit  delta  detected
#>   series1  0.5  20.25  0.5041   0.64      TRUE
```

Every monitor here alarms within about 15 points of the true bubble
start (150), none before it – that “never before `T*` (or the true
start)” property is exactly what each function’s own test suite checks
under the null. Alarm *timing* differs by design:
[`monitor()`](https://kvasilopoulos.github.io/exuber/reference/monitor.md)’s
ADF-family statistics tend to detect mid-sample bubbles fastest (the
literature’s own finding, e.g. Kurozumi 2020/2021), while CUSUM-type
detectors
([`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md),
[`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md))
are typically slower but computationally simpler and don’t need a
bootstrap.

## Which to reach for

- Fastest detection, willing to pay for a wild bootstrap per call:
  `monitor(boundary = "bootstrap")` (the default).
- Same statistic, no bootstrap, an off-the-shelf published constant
  instead: `monitor(boundary = "kurozumi")` or `boundary = "fluc"`.
- A simpler CUSUM-based alternative with its own closed-form boundary:
  [`monitor_cusum()`](https://kvasilopoulos.github.io/exuber/reference/monitor_cusum.md),
  or
  [`monitor_lbi()`](https://kvasilopoulos.github.io/exuber/reference/monitor_lbi.md)
  for Breitung & Diegel’s locally-best-invariant version (`c_bar > 0`
  trades a little size for power against slow-building bubbles).
- Monitoring a specific quantile of the distribution rather than the
  mean behavior:
  [`monitor_quantile()`](https://kvasilopoulos.github.io/exuber/reference/monitor_quantile.md).
