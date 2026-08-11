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
  1.8/\sqrt(T))T\\, where T denotes the sample size).

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

Hafner, C. M. (2020). Testing for bubbles in cryptocurrencies with
time-varying volatility. Journal of Financial Econometrics, 18(2),
233-249.

## See also

[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
for Monte Carlo critical values and
[`radf_sb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sb_cv.md)
for sieve bootstrap critical values.

## Examples

``` r
# \donttest{
# Default minimum window
wb <- radf_wb_cv(sim_data)

tidy(wb)
#> # A tibble: 15 × 5
#>    id    sig       adf   sadf gsadf
#>    <fct> <fct>   <dbl>  <dbl> <dbl>
#>  1 psy1  90    -0.647   1.42   2.98
#>  2 psy2  90    -0.651   2.86   3.84
#>  3 evans 90    -0.571   5.65   8.17
#>  4 div   90    -0.406   0.994  1.66
#>  5 blan  90    -0.307   3.03   5.82
#>  6 psy1  95    -0.460   2.21   3.65
#>  7 psy2  95    -0.507   3.63   4.52
#>  8 evans 95    -0.388   7.68   9.78
#>  9 div   95    -0.0553  1.27   1.91
#> 10 blan  95    -0.0459  4.34   7.57
#> 11 psy1  99    -0.0646  3.16   4.76
#> 12 psy2  99    -0.263   4.87   5.82
#> 13 evans 99    -0.0748 11.3   15.6 
#> 14 div   99     0.724   1.83   2.37
#> 15 blan  99     0.276   6.77  13.1 

# Change the minimum window and the number of bootstraps
wb2 <- radf_wb_cv(sim_data, nboot = 600, minw = 20)

tidy(wb2)
#> # A tibble: 15 × 5
#>    id    sig       adf   sadf gsadf
#>    <fct> <fct>   <dbl>  <dbl> <dbl>
#>  1 psy1  90    -0.545   1.47   2.69
#>  2 psy2  90    -0.634   2.98   3.79
#>  3 evans 90    -0.548   4.80   8.03
#>  4 div   90    -0.282   0.985  1.69
#>  5 blan  90    -0.322   3.22   6.34
#>  6 psy1  95    -0.381   1.99   3.31
#>  7 psy2  95    -0.506   3.63   4.85
#>  8 evans 95    -0.383   7.46  10.3 
#>  9 div   95     0.0310  1.32   2.04
#> 10 blan  95    -0.0760  4.21   8.03
#> 11 psy1  99     0.0632  3.03   4.50
#> 12 psy2  99    -0.199   5.44   6.36
#> 13 evans 99    -0.0225 13.5   14.3 
#> 14 div   99     0.372   1.72   2.84
#> 15 blan  99     0.646   6.74  14.1 

# Simulate distribution
wdist <- radf_wb_distr(sim_data)

autoplot(wdist)

# }
```
