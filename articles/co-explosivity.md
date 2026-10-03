# Bivariate Bubble Relationships: cobubble_test() and contagion_reg()

``` r

library(exuber)
```

## Two different questions about two series

Both functions look at a pair of series that each contain, or might
contain, an explosive episode. They ask different questions.

- [`cobubble_test()`](https://kvasilopoulos.github.io/exuber/reference/cobubble_test.md)
  (Evripidou, Harvey, Leybourne & Sollis 2022) is a formal hypothesis
  test of whether the explosive episodes of `y` and `x` are the same
  episode, that is, whether `y_t - alpha - beta * x_{t-i}` is stationary
  for some lag or lead `i`. It is a KPSS-type test whose null hypothesis
  is co-explosivity (a stationary residual). Rejecting the null
  therefore means that the bubbles in the two series do not come from
  the same underlying process.
- [`contagion_reg()`](https://kvasilopoulos.github.io/exuber/reference/contagion_reg.md)
  (Greenaway-McGrevy & Phillips 2016) does no formal inference. It
  estimates a time-varying contagion coefficient `delta_2(r)` with a
  Nadaraya-Watson kernel regression. At each point `r` in the sample,
  the coefficient measures how strongly the fixed-window AR(1)
  coefficient of a “peripheral” series `y` moves with the coefficient of
  a “core” series.

Neither function returns the `radf_obj` class (see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)).

## `cobubble_test()`: are these the same bubble?

[`sim_coexplosive()`](https://kvasilopoulos.github.io/exuber/reference/sim_coexplosive.md)
implements the data generating process of Evripidou et al.: `y` is a
linear function of an explosive `x` plus noise. The pair is
co-explosive, so a correct test should not reject:

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

For contrast, `sim_data$psy1` and `sim_data$psy2` are simulated
independently, so by construction they share no bubble process:

``` r

cobubble_test(sim_data$psy1, sim_data$psy2, nboot = 199, seed = 1)
#> 
#> ── cobubble_test (lag = -2, nboot = 199) ───────────────────────────────────────
#> 
#> S = 1.533, cv(95%) = 0.2998, p-value = 0
#> Co-explosivity rejected at the 5% level.
```

Here `S` clearly exceeds its wild-bootstrap critical value, which is
robust to heteroskedasticity, and co-explosivity is rejected, as it
should be.

## `contagion_reg()`: how strongly do they co-move, and when?

For the co-explosive pair, the AR(1) coefficient of `y` follows that of
`x` almost one for one throughout the sample:

``` r

cr <- contagion_reg(xy$y, xy$x, d = 0)
cr
#> 
#> ── contagion_reg (n = 100, S = 33, d = 0, h = 0.6567) ──────────────────────────
#> 
#> delta_2(r) range: [0.948, 0.965]
```

`cr$delta2` holds the whole estimated path over `cr$r_grid`, while
[`print()`](https://rdrr.io/r/base/print.html) shows only its range:

``` r

plot(cr$r_grid, cr$delta2, type = "l",
     xlab = "r (fraction of sample)", ylab = expression(delta[2](r)),
     main = "Estimated time-varying contagion coefficient")
```

![](co-explosivity_files/figure-html/contagion-plot-1.png)

For contrast, the two independent `sim_data` series give a coefficient
path that stays far below one:

``` r

cr_null <- contagion_reg(sim_data$psy1, sim_data$psy2, d = 0)
range(cr_null$delta2)
#> [1] 0.1628972 0.1823787
range(cr$delta2)
#> [1] 0.9476613 0.9652861
```

## Which to reach for

- If you want a yes or no answer, with a critical value, to the question
  of whether two series share the same explosive episode, use
  [`cobubble_test()`](https://kvasilopoulos.github.io/exuber/reference/cobubble_test.md).
- If you want to see how the comovement between the AR coefficients of
  two series changes over the sample, for example to watch contagion
  build up before a joint collapse, and you do not need a formal test,
  use
  [`contagion_reg()`](https://kvasilopoulos.github.io/exuber/reference/contagion_reg.md).
