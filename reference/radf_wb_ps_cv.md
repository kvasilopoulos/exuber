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
  1.8/\sqrt{T})T\\, where T denotes the sample size).

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
[doi:10.1111/iere.12132](https://doi.org/10.1111/iere.12132)

## See also

[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
for the Harvey et al. (2016) wild bootstrap,
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
[`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)

## Examples

``` r
# \donttest{
# Default minimum window
wb <- radf_wb_ps_cv(sim_data)

tidy(wb)
#> # A tibble: 15 × 5
#>    id    sig       adf  sadf gsadf
#>    <fct> <fct>   <dbl> <dbl> <dbl>
#>  1 psy1  90    -0.423   1.14  2.13
#>  2 psy2  90    -0.426   1.44  2.33
#>  3 evans 90    -0.431   1.42  2.67
#>  4 div   90    -0.218   1.10  1.90
#>  5 blan  90    -0.493   1.25  2.27
#>  6 psy1  95    -0.0690  1.59  2.49
#>  7 psy2  95    -0.151   2.02  3.01
#>  8 evans 95    -0.109   2.02  3.27
#>  9 div   95     0.119   1.54  2.38
#> 10 blan  95    -0.168   1.64  2.73
#> 11 psy1  99     0.788   2.81  3.86
#> 12 psy2  99     0.735   3.38  4.77
#> 13 evans 99     0.504   3.29  5.21
#> 14 div   99     0.819   2.05  3.25
#> 15 blan  99     0.522   2.40  3.92

# Change the minimum window and the number of bootstraps
wb2 <- radf_wb_ps_cv(sim_data, nboot = 600, minw = 20)

tidy(wb2)
#> # A tibble: 15 × 5
#>    id    sig         adf  sadf gsadf
#>    <fct> <fct>     <dbl> <dbl> <dbl>
#>  1 psy1  90    -0.382     1.26  2.08
#>  2 psy2  90    -0.398     1.40  2.34
#>  3 evans 90    -0.399     1.32  2.63
#>  4 div   90    -0.365     1.16  2.01
#>  5 blan  90    -0.337     1.45  2.35
#>  6 psy1  95    -0.124     1.75  2.51
#>  7 psy2  95    -0.0329    1.78  2.94
#>  8 evans 95     0.0270    1.87  3.19
#>  9 div   95    -0.0363    1.54  2.46
#> 10 blan  95    -0.000135  1.91  3.04
#> 11 psy1  99     0.563     2.51  3.16
#> 12 psy2  99     0.453     2.84  4.67
#> 13 evans 99     0.729     3.17  4.54
#> 14 div   99     0.455     2.14  3.56
#> 15 blan  99     0.967     2.94  4.01

# Simulate distribution
wdist <- radf_wb_ps_distr(sim_data)

autoplot(wdist)


# Apply the critical values to actual data
rsim_data <- radf(sim_data, minw = 20)
autoplot(rsim_data, cv = wb2)

# }
```
