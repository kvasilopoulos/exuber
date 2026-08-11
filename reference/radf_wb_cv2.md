# Wild Bootstrap Critical Values

`radf_wb_cv` performs the Phillips & Shi (2020) wild bootstrap
re-sampling scheme, which is asymptotically robust to non-stationary
volatility, to generate critical values for the recursive unit root
tests. `radf_wb_distr2` computes the distribution.

## Usage

``` r
radf_wb_cv2(
  data,
  minw = NULL,
  nboot = 500L,
  adflag = 0,
  type = c("fixed", "aic", "bic"),
  tb = NULL,
  seed = NULL
)

radf_wb_distr2(
  data,
  minw = NULL,
  nboot = 500L,
  adflag = 0,
  type = c("fixed", "aic", "bic"),
  tb = NULL,
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

- adflag:

  A positive integer. Number of lags when type is "fixed" or number of
  max lags when type is either "aic" or "bic".

- type:

  Character. "fixed" for fixed lag, "aic" or "bic" for automatic lag
  selection according to the criterion.

- tb:

  A positive integer. The simulated sample size.

- seed:

  An object specifying if and how the random number generator (rng)
  should be initialized. Either NULL or an integer will be used in a
  call to `set.seed` before simulation. If set, the value is saved as
  "seed" attribute of the returned value. The default, NULL, will not
  change rng state, and return .Random.seed as the "seed" attribute.
  Results are reproducible across the parallel and non-parallel option
  when the same seed is used.

## Value

For `radf_wb_cv2` a list that contains the critical values for the ADF,
BADF, BSADF and GSADF tests. For `radf_wb_distr` a list that contains
the ADF, SADF and GSADF distributions.

## References

Phillips, P. C., & Shi, S. (2020). Real time monitoring of asset
markets: Bubbles and crises. In Handbook of Statistics (Vol. 42, pp.
61-80). Elsevier.

Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for Multiple
Bubbles: Historical Episodes of Exuberance and Collapse in the S&P 500.
International Economic Review, 56(4), 1043-1078.

## See also

[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
for Monte Carlo critical values and
[`radf_sb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sb_cv.md)
for sieve bootstrap critical values.

## Examples

``` r
# \donttest{
# Default minimum window
wb <- radf_wb_cv2(sim_data)

tidy(wb)
#> # A tibble: 15 × 5
#>    id    sig       adf  sadf gsadf
#>    <fct> <fct>   <dbl> <dbl> <dbl>
#>  1 psy1  90    -0.311   1.27  2.17
#>  2 psy2  90    -0.259   1.34  2.49
#>  3 evans 90    -0.459   1.29  2.87
#>  4 div   90    -0.457   1.05  1.88
#>  5 blan  90    -0.436   1.24  2.16
#>  6 psy1  95     0.146   1.62  2.68
#>  7 psy2  95     0.0484  1.97  2.94
#>  8 evans 95    -0.129   2.05  3.51
#>  9 div   95    -0.210   1.45  2.25
#> 10 blan  95    -0.136   1.64  2.66
#> 11 psy1  99     1.05    2.67  3.35
#> 12 psy2  99     0.454   3.30  4.14
#> 13 evans 99     0.597   3.26  5.68
#> 14 div   99     0.541   2.01  3.11
#> 15 blan  99     0.599   2.36  3.68

# Change the minimum window and the number of bootstraps
wb2 <- radf_wb_cv2(sim_data, nboot = 600, minw = 20)

tidy(wb2)
#> # A tibble: 15 × 5
#>    id    sig       adf  sadf gsadf
#>    <fct> <fct>   <dbl> <dbl> <dbl>
#>  1 psy1  90    -0.383   1.26  1.98
#>  2 psy2  90    -0.385   1.41  2.24
#>  3 evans 90    -0.495   1.54  2.45
#>  4 div   90    -0.422   1.04  2.02
#>  5 blan  90    -0.381   1.37  2.24
#>  6 psy1  95    -0.137   1.73  2.36
#>  7 psy2  95    -0.178   1.78  2.70
#>  8 evans 95    -0.137   1.97  3.09
#>  9 div   95    -0.0960  1.42  2.54
#> 10 blan  95    -0.0700  1.86  2.72
#> 11 psy1  99     0.565   2.63  3.40
#> 12 psy2  99     0.475   3.09  4.33
#> 13 evans 99     0.445   2.86  4.47
#> 14 div   99     0.464   2.15  3.19
#> 15 blan  99     0.540   2.72  3.98

# Simulate distribution
wdist <- radf_wb_distr(sim_data)

autoplot(wdist)

# }
```
