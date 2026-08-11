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

## Status

**\[experimental\]**

## References

Guo, G., Sun, Y., & Wang, S. (2019). Testing for moderate explosiveness.
The Econometrics Journal, 22(3), 279-303.

Phillips, P. C. B., & Magdalinos, T. (2007). Limit theory for moderate
deviations from a unit root. Journal of Econometrics, 136(1), 115-130.
