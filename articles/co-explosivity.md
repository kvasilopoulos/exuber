# Bivariate Bubble Relationships: cobubble_test() and contagion_reg()

``` r

library(exuber)
```

## Two different questions about two series

Both functions here look at a *pair* of series that each contain (or
might contain) an explosive episode, but they ask different questions
and answer them differently:

- [`cobubble_test()`](https://kvasilopoulos.github.io/exuber/reference/cobubble_test.md)
  (Evripidou, Harvey, Leybourne & Sollis 2022) is a formal **hypothesis
  test**: are `y` and `x`‘s explosive episodes the *same* episode,
  i.e. is `y_t - alpha - beta * x_{t-i}` stationary for some lag/lead
  `i`? This is a KPSS-type test – the null hypothesis is co-explosivity
  (stationary residual), so *rejecting* means the two series’ bubbles
  are not the same underlying process.
- [`contagion_reg()`](https://kvasilopoulos.github.io/exuber/reference/contagion_reg.md)
  (Greenaway-McGrevy & Phillips 2016) performs no formal inference at
  all. It estimates a **time-varying** contagion coefficient
  `delta_2(r)`: at each point `r` in the sample, how strongly a
  “peripheral” series `y`‘s (fixed-window) AR(1) coefficient co-moves
  with a “core” series’ own coefficient, via a Nadaraya-Watson kernel
  regression.

Neither carries the `radf_obj` class – see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md).

## `cobubble_test()`: are these the same bubble?

[`sim_coexplosive()`](https://kvasilopoulos.github.io/exuber/reference/sim_coexplosive.md)
is Evripidou et al.’s own DGP: `y` is a linear function of an explosive
`x` (plus noise), so the pair *is* co-explosive and a correct test
should not reject:

``` r

xy <- sim_coexplosive(n = 100, seed = 123)
res <- cobubble_test(xy$y, xy$x, nboot = 199, seed = 1)
res
#> 
#> ── cobubble_test (lag = 0, nboot = 199) ────────────────────────────────────────
#> 
#> S = 0.2364, cv(95%) = 0.4048, p-value = 0.1508
#> Co-explosivity not rejected at the 5% level.
```

For contrast, `sim_data$psy1` and `sim_data$psy2` are independently
simulated explosive episodes, sharing no bubble process by construction:

``` r

cobubble_test(sim_data$psy1, sim_data$psy2, nboot = 199, seed = 1)
#> 
#> ── cobubble_test (lag = -2, nboot = 199) ───────────────────────────────────────
#> 
#> S = 1.533, cv(95%) = 0.2998, p-value = 0
#> Co-explosivity rejected at the 5% level.
```

Here `S` comfortably exceeds its (wild-bootstrap,
heteroskedasticity-robust) critical value and co-explosivity is rejected
– correctly.

## `contagion_reg()`: how strongly do they co-move, and when?

On the co-explosive pair, `y`’s AR(1) coefficient tracks `x`’s almost
one-for-one throughout the sample:

``` r

cr <- contagion_reg(xy$y, xy$x, d = 0)
cr
#> 
#> ── contagion_reg (n = 100, S = 33, d = 0, h = 0.6567) ──────────────────────────
#> 
#> delta_2(r) range: [0.948, 0.965]
```

`cr$delta2` is the full estimated path over `cr$r_grid`, not just the
range [`print()`](https://rdrr.io/r/base/print.html) shows:

``` r

plot(cr$r_grid, cr$delta2, type = "l",
     xlab = "r (fraction of sample)", ylab = expression(delta[2](r)),
     main = "Estimated time-varying contagion coefficient")
```

![](co-explosivity_files/figure-html/contagion-plot-1.png)

For contrast, the two independent `sim_data` series give a coefficient
path sitting far below one:

``` r

cr_null <- contagion_reg(sim_data$psy1, sim_data$psy2, d = 0)
range(cr_null$delta2)
#> [1] 0.1628972 0.1823787
range(cr$delta2)
#> [1] 0.9476613 0.9652861
```

## Which to reach for

- Want a yes/no answer, with a critical value, to “do these two series
  share the same explosive episode”:
  [`cobubble_test()`](https://kvasilopoulos.github.io/exuber/reference/cobubble_test.md).
- Want to see *how* the strength of comovement between two series’ AR
  coefficients evolves over the sample (e.g. to visualize contagion
  building up before a joint collapse), with no formal test attached:
  [`contagion_reg()`](https://kvasilopoulos.github.io/exuber/reference/contagion_reg.md).
