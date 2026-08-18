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
#>    id    sig        adf  sadf gsadf
#>    <fct> <fct>    <dbl> <dbl> <dbl>
#>  1 psy1  90    -0.431    1.15  2.17
#>  2 psy2  90    -0.336    1.44  2.50
#>  3 evans 90    -0.369    1.50  2.93
#>  4 div   90    -0.345    1.04  2.03
#>  5 blan  90    -0.371    1.24  2.49
#>  6 psy1  95    -0.0538   1.45  2.60
#>  7 psy2  95    -0.00555  1.90  2.85
#>  8 evans 95    -0.0630   2.08  3.71
#>  9 div   95     0.0207   1.47  2.38
#> 10 blan  95    -0.0437   1.89  3.00
#> 11 psy1  99     0.723    2.24  3.54
#> 12 psy2  99     0.743    2.92  4.26
#> 13 evans 99     0.546    3.13  5.92
#> 14 div   99     0.600    2.36  3.75
#> 15 blan  99     0.316    3.41  4.04

# Change the minimum window and the number of bootstraps
wb2 <- radf_wb_cv2(sim_data, nboot = 600, minw = 20)

tidy(wb2)
#> # A tibble: 15 × 5
#>    id    sig        adf  sadf gsadf
#>    <fct> <fct>    <dbl> <dbl> <dbl>
#>  1 psy1  90    -0.344    1.13  2.03
#>  2 psy2  90    -0.365    1.33  2.32
#>  3 evans 90    -0.391    1.45  2.82
#>  4 div   90    -0.322    1.14  2.08
#>  5 blan  90    -0.400    1.34  2.50
#>  6 psy1  95    -0.0220   1.47  2.42
#>  7 psy2  95    -0.0489   1.85  2.90
#>  8 evans 95    -0.158    1.96  3.42
#>  9 div   95    -0.00272  1.54  2.48
#> 10 blan  95    -0.0607   1.88  2.99
#> 11 psy1  99     0.528    2.08  3.35
#> 12 psy2  99     0.560    2.57  3.98
#> 13 evans 99     0.316    3.43  5.71
#> 14 div   99     0.501    2.36  3.11
#> 15 blan  99     0.545    2.83  4.54

# Simulate distribution
wdist <- radf_wb_distr(sim_data)

autoplot(wdist)

# }
```
