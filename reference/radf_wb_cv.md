# Wild Bootstrap Critical Values

`radf_wb_cv` performs the Harvey et al. (2016) wild bootstrap
re-sampling scheme, which is asymptotically robust to non-stationary
volatility, to generate critical values for the recursive unit root
tests. `radf_wb_distr` computes the distribution.

## Usage

``` r
radf_wb_cv(
  data,
  minw = NULL,
  nboot = 500L,
  dist_rad = FALSE,
  dist_skew = FALSE,
  seed = NULL
)

radf_wb_distr(
  data,
  minw = NULL,
  nboot = 500L,
  dist_rad = FALSE,
  dist_skew = FALSE,
  seed = NULL
)
```

## Arguments

- data:

  A univariate or multivariate numeric time series object, a numeric
  vector or matrix, or a data.frame. A column may have leading and/or
  trailing `NA` values (an uneven/unbalanced panel where series enter or
  exit the sample at different times) – those periods are filled with
  `NA` in `badf`/`bsadf` and excluded from that series' `adf`/`sadf`/
  `gsadf`. Interior `NA` values (a gap in the middle of a series) are
  not supported. When any series is padded this way, the panel statistic
  (`bsadf_panel`/`gsadf_panel`) is not available and is returned as
  `NA`, with a warning.

- minw:

  A positive integer. The minimum window size (default = \\(0.01 +
  1.8/\sqrt{T})T\\, where T denotes the sample size).

- nboot:

  A positive integer. Number of bootstraps (default = 500L).

- dist_rad:

  Logical. If TRUE then the Rademacher distribution will be used.

- dist_skew:

  Logical. If TRUE, use Hafner (2020)'s fixed right-skewed multiplier
  distribution instead of the (default) standard normal or
  (`dist_rad = TRUE`) Rademacher one – appropriate when the series'
  return distribution is itself notably right-skewed (e.g.
  cryptocurrency returns, the paper's own application). At most one of
  `dist_rad` and `dist_skew` may be `TRUE`.

- seed:

  An object specifying if and how the random number generator (rng)
  should be initialized. Either NULL or an integer will be used in a
  call to `set.seed` before simulation. If set, the value is saved as
  "seed" attribute of the returned value. The default, NULL, will not
  change rng state, and return .Random.seed as the "seed" attribute.
  Results are reproducible across the parallel and non-parallel option
  when the same seed is used.

## Value

For `radf_wb_cv` a list that contains the critical values for the ADF,
BADF, BSADF and GSADF tests. For `radf_wb_distr` a list that contains
the ADF, SADF and GSADF distributions.

## Details

This approach involves applying a wild bootstrap re-sampling scheme to
construct the bootstrap analogue of the Phillips et al. (2015) test
which is asymptotically robust to non-stationary volatility.

## References

Harvey, D. I., Leybourne, S. J., Sollis, R., & Taylor, A. M. R. (2016).
Tests for explosive financial bubbles in the presence of non-stationary
volatility. Journal of Empirical Finance, 38(Part B), 548-574.

Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for Multiple
Bubbles: Historical Episodes of Exuberance and Collapse in the S&P 500.
International Economic Review, 56(4), 1043-1078.
[doi:10.1111/iere.12132](https://doi.org/10.1111/iere.12132)

Hafner, C. M. (2020). Testing for bubbles in cryptocurrencies with
time-varying volatility. Journal of Financial Econometrics, 18(2),
233-249.

## See also

[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
for Monte Carlo critical values and
[`radf_sb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sb_cv.md)
for sieve bootstrap critical values.

Other critical values:
[`radf_common_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_common_cv.md),
[`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md),
[`radf_recovery_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery_cv.md),
[`radf_sb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sb_cv.md),
[`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md),
[`radf_sign_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md),
[`radf_sign_dm_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm_cv.md),
[`radf_tt_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md),
[`radf_wb_ps_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_ps_cv.md)

## Examples

``` r
# \donttest{
# Volatility triples half-way through the sample: the non-stationary-volatility
# case the wild bootstrap is built for (plain radf_mc_cv() over-rejects here)
y <- sim_psy1(n = 200, seed = 1, e = sim_vol_break(199))
# Default minimum window
wb <- radf_wb_cv(y)

tidy(wb)
#> # A tibble: 3 × 5
#>   id      sig      adf  sadf gsadf
#>   <fct>   <fct>  <dbl> <dbl> <dbl>
#> 1 series1 90    -0.135  3.82  4.32
#> 2 series1 95     0.276  4.81  5.22
#> 3 series1 99     1.16   6.57  7.24

# Change the minimum window and the number of bootstraps
wb2 <- radf_wb_cv(y, nboot = 600, minw = 20)

tidy(wb2)
#> # A tibble: 3 × 5
#>   id      sig       adf  sadf gsadf
#>   <fct>   <fct>   <dbl> <dbl> <dbl>
#> 1 series1 90    -0.209   3.98  4.52
#> 2 series1 95     0.0593  5.02  5.37
#> 3 series1 99     0.913   7.01  7.39

# Simulate distribution
wdist <- radf_wb_distr(y)

autoplot(wdist)


# Apply the critical values to actual data
rsim_data <- radf(y, minw = 20)
autoplot(rsim_data, cv = wb2)

# }
```
