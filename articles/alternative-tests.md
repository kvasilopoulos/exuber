# Alternative Tests: lbi_test(), ssu_test(), quantile_test()

``` r

library(exuber)
```

## Why not just use `radf()`

[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)’s
GSADF statistic tests one specific alternative: a fixed explosive AR(1)
root.
[`lbi_test()`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md),
[`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md)
and
[`quantile_test()`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)
are standalone hypothesis tests – not built on
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)’s
recursive core, and not fit into the
[`summary()`](https://rdrr.io/r/base/summary.html)/[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/[`tidy()`](https://generics.r-lib.org/reference/tidy.html)/[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
pipeline (see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md))
– each targeting a *different* alternative where GSADF-style tests can
lose power:

| Function | Paper | Alternative it targets |
|----|----|----|
| [`lbi_test()`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md) | Breitung & Diegel (2025) | A fixed explosive root, tested with the locally-best-invariant statistic for that specific alternative (so it can out-power GSADF exactly there). |
| [`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md) | Kurozumi & Nishi (2025) | A **stochastically varying** explosive coefficient – the root itself has a random component, not a fixed value. |
| [`quantile_test()`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md) | Wu, Shi & Wu (2025) | Explosiveness in the `tau`-th conditional quantile of `y_t` on `y_{t-1}`, not the conditional mean. |

## Same alternative, `lbi_test()` detects

``` r

y <- sim_psy1(n = 60, te = 1, tf = 60, c = 0.03, alpha = 0, seed = 1) # fixed rho = 1.03 throughout
lbi_test(y)
#> 
#> ── lbi_test (n = 60, level = 95%) ──────────────────────────────────────────────
#> 
#>    series  stat   crit  detected
#>   series1  6.12  1.645      TRUE
```

## A different alternative: where `lbi_test()` misses and `ssu_test()` doesn’t

[`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md)
is designed for a root that itself varies stochastically over time, not
a fixed one:

[`sim_psy1()`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)’s
`coef_noise`/`coef_a` arguments generate exactly that alternative,
`rho_t = 1 + c/n + coef_a * u_t / sqrt(n)`, a random root rather than a
fixed one:

``` r

y <- sim_psy1(n = 150, te = 75, tf = 150, c = 3, alpha = 1, seed = 2001,
              coef_noise = rnorm(149), coef_a = 4)
```

``` r

ssu_test(y, level = 0.95)
#> 
#> ── ssu_test (n = 150, minw = 23, level = 95%, crit = 3.3) ──────────────────────
#> 
#>    series   sadf  detected
#>   series1  14.53      TRUE
lbi_test(y)
#> 
#> ── lbi_test (n = 150, level = 95%) ─────────────────────────────────────────────
#> 
#>    series    stat   crit  detected
#>   series1  0.3121  1.645     FALSE
```

[`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md)
detects it;
[`lbi_test()`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md),
built for a fixed root, does not on this same draw. This isn’t a defect
in
[`lbi_test()`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md)
– it’s the whole reason two tests exist: each is the (locally) most
powerful test for its own alternative, and neither dominates the other
everywhere.

## Testing the quantile, not the mean

[`quantile_test()`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md)
picks (or is given) a quantile `tau` and tests for explosiveness there
instead of in the conditional mean – the setting where that pays off is
heavy-tailed innovations, where the conditional-mean regression is least
reliable, so the DGP here drives the PSY bubble with `t(3)` shocks:

``` r

y_t3 <- sim_psy1(n = 100, seed = 1, e = sim_innov(99, dist = "t", df = 3))
quantile_test(y_t3, nrep = 100, seed = 1)
#> 
#> ── quantile_test (n = 100, level = 95%) ────────────────────────────────────────
#> 
#>    series   tau  tstat    crit  delta  detected
#>   series1  0.25  4.684  0.6824  0.379      TRUE
```

`tau = "optimal"` (the default) searches `tau_grid` and reports the
quantile with the strongest signal, shown here fixed at a specific value
for a faster, reproducible example.

## Which to reach for

- A genuinely fixed explosive root, and want a test with power
  advantages over standard GSADF for exactly that case:
  [`lbi_test()`](https://kvasilopoulos.github.io/exuber/reference/lbi_test.md).
- Suspect the explosive root itself is noisy/time-varying rather than
  constant:
  [`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md).
- Suspect explosiveness shows up more in the tails (or a specific
  quantile) of the distribution than in the mean:
  [`quantile_test()`](https://kvasilopoulos.github.io/exuber/reference/quantile_test.md).
- Not sure which alternative applies, or want the most widely used
  benchmark:
  [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)’s
  GSADF remains the default first test to run.
