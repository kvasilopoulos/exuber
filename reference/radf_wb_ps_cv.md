# Wild Bootstrap Critical Values (Phillips & Shi 2020)

`radf_wb_ps_cv` performs the Phillips & Shi (2020) wild bootstrap
re-sampling scheme – fit a null AR model, resample its residuals – which
is asymptotically robust to non-stationary volatility, to generate
critical values for the recursive unit root tests. `radf_wb_ps_distr`
computes the distribution. Unlike
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)'s
Harvey et al. (2016) non-parametric multiplier bootstrap, this one
supports a training-window boundary (`tb`), which is what
[`monitor`](https://kvasilopoulos.github.io/exuber/reference/monitor.md)
uses it for.

## Usage

``` r
radf_wb_ps_cv(
  data,
  minw = NULL,
  nboot = 500L,
  adflag = 0,
  type = c("fixed", "aic", "bic"),
  tb = NULL,
  seed = NULL
)

radf_wb_ps_distr(
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

For `radf_wb_ps_cv` a list that contains the critical values for the
ADF, BADF, BSADF and GSADF tests. For `radf_wb_ps_distr` a list that
contains the ADF, SADF and GSADF distributions.

## References

Phillips, P. C., & Shi, S. (2020). Real time monitoring of asset
markets: Bubbles and crises. In Handbook of Statistics (Vol. 42, pp.
61-80). Elsevier.

Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for Multiple
Bubbles: Historical Episodes of Exuberance and Collapse in the S&P 500.
International Economic Review, 56(4), 1043-1078.

## See also

[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
for the Harvey et al. (2016) wild bootstrap,
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
for Monte Carlo critical values and
[`radf_sb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sb_cv.md)
for sieve bootstrap critical values.

## Examples

``` r
# \donttest{
# Default minimum window
wb <- radf_wb_ps_cv(sim_data)

tidy(wb)
#> # A tibble: 15 × 5
#>    id    sig        adf  sadf gsadf
#>    <fct> <fct>    <dbl> <dbl> <dbl>
#>  1 psy1  90    -0.389   1.27   2.04
#>  2 psy2  90    -0.463   1.36   2.26
#>  3 evans 90    -0.336   1.48   2.69
#>  4 div   90    -0.525   0.972  2.04
#>  5 blan  90    -0.337   1.50   2.45
#>  6 psy1  95    -0.0502  1.65   2.42
#>  7 psy2  95    -0.178   1.96   2.79
#>  8 evans 95    -0.0194  1.98   3.33
#>  9 div   95    -0.0600  1.29   2.54
#> 10 blan  95     0.00651 2.02   3.12
#> 11 psy1  99     0.625   2.82   3.55
#> 12 psy2  99     0.478   2.94   4.19
#> 13 evans 99     0.738   3.34   4.85
#> 14 div   99     0.656   2.33   3.27
#> 15 blan  99     0.646   3.08   3.96

# Change the minimum window and the number of bootstraps
wb2 <- radf_wb_ps_cv(sim_data, nboot = 600, minw = 20)

tidy(wb2)
#> # A tibble: 15 × 5
#>    id    sig       adf  sadf gsadf
#>    <fct> <fct>   <dbl> <dbl> <dbl>
#>  1 psy1  90    -0.439   1.28  2.09
#>  2 psy2  90    -0.382   1.33  2.27
#>  3 evans 90    -0.339   1.56  2.77
#>  4 div   90    -0.405   1.04  1.98
#>  5 blan  90    -0.241   1.14  2.29
#>  6 psy1  95    -0.166   1.76  2.49
#>  7 psy2  95    -0.0192  1.73  2.89
#>  8 evans 95    -0.0303  2.09  3.48
#>  9 div   95    -0.0455  1.37  2.49
#> 10 blan  95     0.121   1.59  2.63
#> 11 psy1  99     0.428   3.03  3.92
#> 12 psy2  99     0.389   2.69  4.05
#> 13 evans 99     0.623   3.37  5.05
#> 14 div   99     0.446   1.99  3.33
#> 15 blan  99     0.820   2.40  3.73

# Simulate distribution
wdist <- radf_wb_ps_distr(sim_data)

autoplot(wdist)

# }
```
