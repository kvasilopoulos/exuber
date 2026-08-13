# Test for Co-explosive Behaviour Between Two Series

`cobubble_test` tests whether two series that each contain an explosive
episode are *co-explosive*: whether a linear combination
`y_t - alpha - beta * x_{t-lag}` is stationary, i.e. whether the
explosive dynamics in `y` and `x` are the same underlying phenomenon
(possibly migrating from one series to the other with a lead or lag)
rather than independent explosive episodes.

## Usage

``` r
cobubble_test(
  y,
  x,
  lag = NULL,
  lags = -6:6,
  nboot = 499L,
  level = 0.05,
  seed = NULL
)
```

## Arguments

- y, x:

  Numeric vectors of equal length, or objects coercible to one via
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html). `x` is the
  (candidate) explosive-episode regressor; `y` is tested for
  co-explosivity with `x_{t-lag}`.

- lag:

  The lead/lag `i` in `x_{t-lag}`. If `NULL` (default), it is estimated
  from `lags` by minimizing the residual variance (Section VI's
  `i_hat`).

- lags:

  Candidate lag values searched when `lag = NULL`. Default `-6:6`, as in
  the paper's own simulation design.

- nboot:

  Number of wild bootstrap replications.

- level:

  Nominal test size (upper-tail rejection region).

- seed:

  Optional seed for the bootstrap draws.

## Value

An object of class `cobubble_test`: a list with the observed statistic
`S`, the (given or estimated) `lag`, the bootstrap critical value `cv`
at `level`, the bootstrap p-value `p_value`, and `reject` (`TRUE` if `S`
exceeds `cv`, i.e. co-explosivity is rejected).

## Details

Unlike
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) (a
right-tailed ADF-family test for the presence of explosiveness), this is
a stationarity (KPSS-type) test: the null hypothesis is co-explosivity,
i.e. that the residuals of `y` regressed on a constant and `x_{t-lag}`
are I(0). Because the null limiting distribution of the statistic
depends on the pattern of heteroskedasticity in the errors (Evripidou,
Harvey, Leybourne & Sollis 2022, Theorem 1), critical values are
obtained via a wild bootstrap that reproduces that same
heteroskedasticity pattern in the bootstrap samples (Theorem 2).

## Note

The critical value is a wild bootstrap of the residuals, computed
internally on every call (Theorem 2) – there is no separate/reusable cv
function for this test.

## Status

**\[experimental\]**

## References

Evripidou, A. C., Harvey, D. I., Leybourne, S. J., & Sollis, R. (2022).
Testing for co-explosive behaviour in financial time series. Oxford
Bulletin of Economics and Statistics, 84(3), 624-650.

## Examples

``` r
# \donttest{
res <- cobubble_test(sim_data$sim_psy1, sim_data$sim_psy2, nboot = 199L, seed = 1)
#> Warning: Unknown or uninitialised column: `sim_psy1`.
#> Warning: Unknown or uninitialised column: `sim_psy2`.
#> Error in y[idx_y]: only 0's may be mixed with negative subscripts
print(res)
#> Error: object 'res' not found
# }
```
