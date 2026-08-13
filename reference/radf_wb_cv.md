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
#>  1 psy1  90    -0.575   1.62   2.81
#>  2 psy2  90    -0.678   3.18   4.08
#>  3 evans 90    -0.643   5.27   6.92
#>  4 div   90    -0.447   0.873  1.66
#>  5 blan  90    -0.281   2.80   6.34
#>  6 psy1  95    -0.336   2.14   3.21
#>  7 psy2  95    -0.504   4.15   4.51
#>  8 evans 95    -0.437   7.06   9.58
#>  9 div   95    -0.116   1.23   2.08
#> 10 blan  95     0.0645  4.06   8.28
#> 11 psy1  99     0.0651  2.88   4.56
#> 12 psy2  99    -0.309   5.22   5.60
#> 13 evans 99    -0.0470 12.5   13.9 
#> 14 div   99     0.861   1.73   2.67
#> 15 blan  99     0.421   6.89  10.9 

# Change the minimum window and the number of bootstraps
wb2 <- radf_wb_cv(sim_data, nboot = 600, minw = 20)

tidy(wb2)
#> # A tibble: 15 × 5
#>    id    sig        adf   sadf gsadf
#>    <fct> <fct>    <dbl>  <dbl> <dbl>
#>  1 psy1  90    -0.573    1.63   3.01
#>  2 psy2  90    -0.606    3.01   3.74
#>  3 evans 90    -0.545    4.86   7.57
#>  4 div   90    -0.448    0.933  1.70
#>  5 blan  90    -0.219    3.12   6.22
#>  6 psy1  95    -0.400    2.28   3.49
#>  7 psy2  95    -0.462    3.50   4.34
#>  8 evans 95    -0.372    7.12   8.86
#>  9 div   95    -0.0633   1.26   2.11
#> 10 blan  95     0.0491   4.69   7.66
#> 11 psy1  99    -0.127    3.12   4.52
#> 12 psy2  99    -0.249    5.28   6.14
#> 13 evans 99    -0.00870 11.7   11.9 
#> 14 div   99     0.590    1.90   2.83
#> 15 blan  99     0.467    7.54  12.6 

# Simulate distribution
wdist <- radf_wb_distr(sim_data)

autoplot(wdist)

# }
```
