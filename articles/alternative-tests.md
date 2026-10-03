# Alternative Tests: lbi_test(), ssu_test(), quantile_test()

``` r

library(exuber)
```

## Why not just use `radf()`

The GSADF statistic in
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
tests against one specific alternative, an explosive AR(1) root that
stays fixed.
[`lbi_test()`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md),
[`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md)
and
[`quantile_test()`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)
are standalone hypothesis tests. They do not use the recursive core of
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md) and
they do not feed into the
[`summary()`](https://rdrr.io/r/base/summary.html),
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
pipeline (see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)).
Each targets a different alternative, one where GSADF-style tests can
lose power.

| Function | Paper | Alternative it targets |
|----|----|----|
| [`lbi_test()`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md) | Breitung & Diegel (2025) | A fixed explosive root, tested with the locally best invariant statistic for that alternative, so it can have more power than GSADF in exactly that case. |
| [`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md) | Kurozumi & Nishi (2025) | A stochastically varying explosive coefficient: the root has a random component and is not a fixed value. |
| [`quantile_test()`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md) | Wu, Shi & Wu (2025) | Explosiveness in the `tau`-th conditional quantile of `y_t` given `y_{t-1}`, instead of in the conditional mean. |

## A fixed root, which `lbi_test()` detects

``` r

y <- sim_psy1(n = 60, te = 1, tf = 60, c = 0.03, alpha = 0, seed = 1) # fixed rho = 1.03 throughout
lbi_test(y)
#> 
#> ── lbi_test (n = 60, sig_lvl = 95%) ────────────────────────────────────────────
#> 
#>    series  stat   crit  detected
#>   series1  6.12  1.645      TRUE
```

## A varying root, which `ssu_test()` detects and `lbi_test()` misses

[`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md)
is designed for a root that varies stochastically over time. The
`coef_noise` and `coef_a` arguments of
[`sim_psy1()`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)
generate this alternative, with
`rho_t = 1 + c/n + coef_a * u_t / sqrt(n)`, so the root is random and
not fixed:

``` r

y <- sim_psy1(n = 150, te = 75, tf = 150, c = 3, alpha = 1, seed = 2001,
              coef_noise = rnorm(149), coef_a = 4)
```

``` r

ssu_test(y, sig_lvl = 95)
#> 
#> ── ssu_test (SSU, n = 150, minw = 23, sig_lvl = 95%, crit = 3.3) ───────────────
#> 
#>    series   sadf  detected
#>   series1  15.02      TRUE
lbi_test(y)
#> 
#> ── lbi_test (n = 150, sig_lvl = 95%) ───────────────────────────────────────────
#> 
#>    series    stat   crit  detected
#>   series1  0.3121  1.645     FALSE
```

On this draw
[`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md)
detects the bubble and
[`lbi_test()`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md),
which is built for a fixed root, does not. This is not a defect of
[`lbi_test()`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md).
Each test is the (locally) most powerful one against its own
alternative, and neither dominates the other everywhere, which is why
both exist.

## Testing a quantile instead of the mean

[`quantile_test()`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)
picks a quantile `tau` (or takes one from you) and tests for
explosiveness there instead of in the conditional mean. This pays off
with heavy-tailed innovations, where the conditional-mean regression is
least reliable, so the example below drives the PSY bubble with `t(3)`
shocks:

``` r

y_t3 <- sim_psy1(n = 100, seed = 1, e = sim_innov(99, dist = "t", df = 3))
quantile_test(y_t3, nrep = 100, seed = 1)
#> 
#> ── quantile_test (n = 100, sig_lvl = 95%) ──────────────────────────────────────
#> 
#>    series   tau  tstat    crit  delta  detected
#>   series1  0.25  4.684  0.6824  0.379      TRUE
```

By default (`tau = "optimal"`) the function searches `tau_grid` and
reports the quantile with the strongest signal. The example above fixes
the quantile at a specific value so that it runs faster and can be
reproduced.

## Which to reach for

- If you believe the explosive root is fixed and want more power than
  GSADF in that case, use
  [`lbi_test()`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md).
- If you suspect the explosive root is noisy or varies over time, use
  [`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md).
- If you suspect explosiveness shows up in the tails of the
  distribution, or at a specific quantile, more than in the mean, use
  [`quantile_test()`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md).
- If you are unsure which alternative applies, or want the most widely
  used benchmark, start with the GSADF test in
  [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md).
