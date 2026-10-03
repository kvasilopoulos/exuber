# Test for Co-explosive Behaviour Between Two Series

`cobubble_test` tests whether two series that each contain an explosive
episode are *co-explosive*. That is, it tests whether the linear
combination `y_t - alpha - beta * x_{t-lag}` is stationary, so that the
explosive dynamics in `y` and `x` are the same underlying phenomenon,
possibly migrating from one series to the other with a lead or lag, and
not independent explosive episodes.

## Usage

``` r
cobubble_test(
  y,
  x,
  lag = NULL,
  lag_grid = -6:6,
  nboot = 499L,
  sig_lvl = 95,
  seed = NULL
)
```

## Arguments

- y, x:

  Numeric vectors of equal length, or objects that
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html) can coerce to
  one. `x` is the candidate regressor with the explosive episode, and
  `y` is tested for co-explosivity with `x_{t-lag}`.

- lag:

  The lead or lag `i` in `x_{t-lag}`. If `NULL` (default), it is
  estimated from `lag_grid` by minimizing the residual variance (`i_hat`
  in Section VI).

- lag_grid:

  Candidate lag values searched when `lag = NULL`. The default `-6:6`
  follows the simulation design of the paper.

- nboot:

  Number of wild bootstrap replications.

- sig_lvl:

  Significance level, on the same 0 to 100 scale as the `sig_lvl` of
  [`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
  (default `95`, which gives a 5\\ upper-tail rejection region).

- seed:

  Optional seed for the bootstrap draws.

## Value

An object of class `cobubble_test_obj`: a list with the observed
statistic `S`, the (given or estimated) `lag`, the bootstrap critical
value `cv` at `sig_lvl`, the bootstrap p-value `p_value` and `reject`,
which is `TRUE` if `S` exceeds `cv`, that is, if co-explosivity is
rejected.

## Details

[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) is a
right-tailed ADF-family test for the presence of explosiveness. This
function is a stationarity (KPSS-type) test instead. The null hypothesis
is co-explosivity, that is, that the residuals of `y` regressed on a
constant and `x_{t-lag}` are I(0). The null limiting distribution of the
statistic depends on the pattern of heteroskedasticity in the errors
(Evripidou, Harvey, Leybourne & Sollis 2022, Theorem 1). The critical
values therefore come from a wild bootstrap that reproduces this pattern
of heteroskedasticity in the bootstrap samples (Theorem 2).

## Note

The critical value comes from a wild bootstrap of the residuals (Theorem
2), which is computed internally in each call. There is no separate,
reusable cv function for this test.

The function returns its own class and not `radf_obj`, so it does not
work with [`summary()`](https://rdrr.io/r/base/summary.html),
`\link{datestamp}` and `tidy`. It has its own
[`print()`](https://rdrr.io/r/base/print.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods instead. [`print()`](https://rdrr.io/r/base/print.html) shows
the statistic, the critical value and the p-value. See
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for which functions fit the shared pipeline and which do not.

## Status

**\[experimental\]**

## References

Evripidou, A. C., Harvey, D. I., Leybourne, S. J., & Sollis, R. (2022).
Testing for co-explosive behaviour in financial time series. Oxford
Bulletin of Economics and Statistics, 84(3), 624-650.

## See also

Other multivariate:
[`contagion_reg()`](https://kvasilopoulos.github.io/exuber/reference/contagion_reg.md),
[`radf_common()`](https://kvasilopoulos.github.io/exuber/reference/radf_common.md)

## Examples

``` r
# \donttest{
# A co-explosive pair (the process of Evripidou et al.), which is not rejected
xy <- sim_coexplosive(n = 100, seed = 123)
res <- cobubble_test(xy$y, xy$x, nboot = 199L, seed = 1)
print(res)
#> 
#> ── cobubble_test (lag = 0, nboot = 199) ────────────────────────────────────────
#> 
#> S = 0.2364, cv(95%) = 0.4048, p-value = 0.1508
#> Co-explosivity not rejected at the 5% level.
#> 

# Force a specific lead or lag instead of estimating it
res_lag0 <- cobubble_test(xy$y, xy$x, lag = 0L, nboot = 199L, seed = 1)
print(res_lag0)
#> 
#> ── cobubble_test (lag = 0, nboot = 199) ────────────────────────────────────────
#> 
#> S = 0.2364, cv(95%) = 0.4048, p-value = 0.1508
#> Co-explosivity not rejected at the 5% level.
#> 

# Two independent bubbles: co-explosivity correctly rejected
cobubble_test(sim_data$psy1, sim_data$psy2, nboot = 199L, seed = 1)
#> 
#> ── cobubble_test (lag = -2, nboot = 199) ───────────────────────────────────────
#> 
#> S = 1.533, cv(95%) = 0.2998, p-value = 0
#> Co-explosivity rejected at the 5% level.
#> 

# Plot the two series that are tested for co-explosivity
autoplot(res)

# }
```
