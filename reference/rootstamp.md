# Confidence Interval and Doubling Time for an Explosive Root

Fits a no-intercept AR(1) regression \\y_t = \rho y\_{t-1} +
\epsilon_t\\ (Phillips & Magdalinos 2007; no intercept, following their
eq. 58, "to exclude the presence of a deterministically explosive
component") and reports \\\hat\rho\\ together with a confidence interval
and implied **doubling time** \\\log(2)/\log(\hat\rho)\\ – the number of
periods for the series to double in magnitude at the estimated growth
rate. Guo, Sun & Wang (2019) show that – unlike the classical
(stationary or unit-root) case – the ordinary t-statistic for
\\\hat\rho\\, estimated by OLS with no intercept, is asymptotically
**standard normal** under i.i.d. errors (and under weakly dependent
errors, with a HAC standard error). This means an ordinary-looking Wald
interval, \\\hat\rho \pm z\_{\alpha/2}\cdot se(\hat\rho)\\, is
asymptotically valid here even though \\\hat\rho \> 1\\ – despite
looking identical in form to a classical (invalid, for an explosive
root) normal-theory interval, the justification is different (Guo, Sun &
Wang's explosive-root CLT, not the classical stationary one).

## Usage

``` r
rootstamp(object, ...)

# Default S3 method
rootstamp(object, level = 0.95, type = c("normal", "cauchy"), ...)

# S3 method for class 'radf_obj'
rootstamp(object, ds, level = 0.95, type = c("normal", "cauchy"), ...)
```

## Arguments

- object:

  For the default method, a numeric vector (the sub-sample to fit –
  already sliced to the episode of interest). For the `radf_obj` method,
  the `radf_obj` that `ds` was computed on.

- ...:

  further arguments passed to methods.

- level:

  Confidence level (default 0.95).

- type:

  `"normal"` (default) for Guo, Sun & Wang's normal-t interval, or
  `"cauchy"` for the Phillips-Magdalinos fixed-root Cauchy interval.

- ds:

  (`radf_obj` method only) A
  [`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
  result computed on `object`. Root inference on a very short episode is
  statistically meaningless – set `min_duration` in that
  [`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
  call to exclude episodes too short for reliable root inference, rather
  than expecting this method to second-guess what counts as "too short".

## Value

The default method returns a `rootstamp_est` object (a list with `rho`,
`se`, `t_stat`, `n`, `rho_ci`, `doubling_time`, `doubling_time_ci`, with
its own [`print()`](https://rdrr.io/r/base/print.html) method).

The `radf_obj` method returns a `rootstamp_episodes` object (a named
list, one element per series in `ds`; the panel sieve-bootstrap case,
whose `ds` entry is named `"panel"` and has no single corresponding
series, is dropped with a warning), each a data frame with one row per
datestamped episode: `Start`, `End`, `rho`, `rho_lower`, `rho_upper`,
`doubling_time`, `doubling_time_lower`, `doubling_time_upper` – also
with its own [`print()`](https://rdrr.io/r/base/print.html) method.

## Details

`type = "cauchy"` instead uses the Phillips & Magdalinos (2007)
fixed-root result (their eq. 27, restating White 1958): for a genuinely
explosive, non-drifting root, \\\frac{\rho^n}{\rho^2-1}(\hat\rho-\rho)\\
converges to a standard Cauchy variate. Plugging in \\\hat\rho\\ for the
unknown \\\rho\\ in the normalization (the usual practice for this kind
of self-normalized pivot) gives \\\hat\rho \pm q\_{\alpha/2}\cdot
(\hat\rho^2-1)/\hat\rho^n\\, with \\q\_{\alpha/2}\\ a standard-Cauchy
quantile. This interval assumes a *fixed* explosive root (no drift, no
unknown localizing rate); the default `"normal"` type is the safer
choice when that assumption is in doubt, since Guo, Sun & Wang's result
allows drift and weak dependence.

Two methods, for two different starting points:

- **Default**: `object` is a numeric vector – the sub-sample to fit
  (e.g. an episode already sliced out by hand, or by position from a
  [`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
  result: `y[from:to]`). Fits once and returns one CI.

- **`radf_obj`**: `object` is the `radf_obj` a
  [`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
  result `ds` was computed on. Runs the default method once per
  datestamped episode, per series, slicing `object`'s own data itself –
  no manual loop needed.

## Note

Neither method's return value carries the `radf_obj` class – even the
`radf_obj` method, which dispatches on that class for its *input*,
returns its own `rootstamp_episodes` class – so `rootstamp()` does not
plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/`\link{datestamp}`/`tidy`/`autoplot`.
See
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the full picture of which functions do and don't fit that pipeline.

## Status

**\[experimental\]**

## References

Guo, G., Sun, Y., & Wang, S. (2019). Testing for moderate explosiveness.
The Econometrics Journal, 22(3), 279-303.

Phillips, P. C. B., & Magdalinos, T. (2007). Limit theory for moderate
deviations from a unit root. Journal of Econometrics, 136(1), 115-130.

## Examples

``` r
set.seed(2026)
burn <- cumsum(rnorm(60))
bubble <- burn[length(burn)] * 1.04^(1:40) + cumsum(rnorm(40, sd = 0.5))
y <- c(burn, bubble)

r <- radf(y, minw = 20)
cv <- radf_mc_cv(length(y), minw = 20, nrep = 300, seed = 4)
ds <- datestamp(r, cv = cv, min_duration = 3)

# default method: one episode, sliced by hand
rootstamp(y[ds[["series1"]]$Start[1]:ds[["series1"]]$End[1]]) # true rho = 1.04
#> 
#> ── rootstamp (n = 17, level = 95%, type = normal) ──────────────────────────────
#> 
#>     rho        se  t_stat  rho_lower  rho_upper  doubling_time  dt_lower
#>   1.042  0.006318   6.639       1.03      1.054          16.87      13.1
#>   dt_upper
#>      23.79
#> 
rootstamp(y[ds[["series1"]]$Start[1]:ds[["series1"]]$End[1]], type = "cauchy")
#> 
#> ── rootstamp (n = 17, level = 95%, type = cauchy) ──────────────────────────────
#> 
#>     rho        se  t_stat  rho_lower  rho_upper  doubling_time  dt_lower
#>   1.042  0.006318   6.639     0.5007      1.583          16.87     1.509
#>   dt_upper
#>     -1.002
#> 


# radf_obj method: every datestamped episode at once
rootstamp(r, ds)
#> 
#> ── rootstamp (level = 95%, type = normal) ──────────────────────────────────────
#> 
#> series1 :
#>   Start End   rho rho_lower rho_upper doubling_time doubling_time_lower
#> 1    83 100 1.042      1.03     1.054         16.87                13.1
#>   doubling_time_upper
#> 1               23.79
#> 
#> 
```
