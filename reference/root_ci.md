# Confidence Interval and Doubling Time for an Explosive Root

Guo, Sun & Wang (2019) show that – unlike the classical (stationary or
unit-root) case – the ordinary t-statistic for the autoregressive root
\\\hat\rho\\ of a (moderately) explosive AR(1), estimated by OLS with no
intercept, is asymptotically **standard normal** under i.i.d. errors
(and under weakly dependent errors, with a HAC standard error). This
means an ordinary-looking Wald interval, \\\hat\rho \pm
z\_{\alpha/2}\cdot se(\hat\rho)\\, is asymptotically valid here even
though \\\hat\rho \> 1\\ – despite looking identical in form to a
classical (invalid, for an explosive root) normal-theory interval, the
justification is different (Guo, Sun & Wang's explosive-root CLT, not
the classical stationary one).

## Usage

``` r
root_ci(x, level = 0.95, type = c("normal", "cauchy"))
```

## Arguments

- x:

  A list as returned by
  [`explosive_root`](https://kvasilopoulos.github.io/exuber/reference/explosive_root.md).

- level:

  Confidence level (default 0.95).

- type:

  `"normal"` (default) for Guo, Sun & Wang's normal-t interval, or
  `"cauchy"` for the Phillips-Magdalinos fixed-root Cauchy interval.

## Value

A list with `rho`, `rho_ci` (length-2 vector), and `doubling_time`,
`doubling_time_ci`.

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

`root_ci` also reports the implied *doubling time*
\\\log(2)/\log(\hat\rho)\\: the number of periods for the bubble to
double in magnitude at the estimated growth rate, with its own interval
obtained by transforming the endpoints of the \\\hat\rho\\ interval
(doubling time is strictly decreasing in \\\rho\\, so the CI's lower and
upper doubling-time bounds come from the upper and lower \\\rho\\
bounds, respectively).

## Note

Returns its own class (not `radf_obj`), so it does not plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/`\link{datestamp}`/`tidy`/`autoplot`
– prints its own confidence-interval summary (this is inference on the
root's magnitude, not a `radf_obj`-shaped test result) – see
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

est <- explosive_root(y, ds[["series1"]]$Start[1], ds[["series1"]]$End[1])
root_ci(est) # true rho = 1.04 -- CI should bracket it
#> $rho
#> [1] 1.041945
#> 
#> $rho_ci
#> [1] 1.029562 1.054328
#> 
#> $doubling_time
#> [1] 16.86941
#> 
#> $doubling_time_ci
#> [1] 13.10209 23.79234
#> 
root_ci(est, type = "cauchy")
#> $rho
#> [1] 1.041945
#> 
#> $rho_ci
#> [1] 0.5007196 1.5831701
#> 
#> $doubling_time
#> [1] 16.86941
#> 
#> $doubling_time_ci
#> [1]  1.508714 -1.002079
#> 
```
