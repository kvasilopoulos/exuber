# Monte Carlo Critical Values

`radf_mc_cv` computes Monte Carlo critical values for the recursive unit
root tests. `radf_mc_distr` computes the distribution.

## Usage

``` r
radf_mc_cv(n, minw = NULL, nrep = 1000L, seed = NULL, lag = 0)

radf_mc_distr(n, minw = NULL, nrep = 1000L, seed = NULL, lag = 0)
```

## Arguments

- n:

  A positive integer. The sample size.

- minw:

  A positive integer. The minimum window size (default = \\(0.01 +
  1.8/\sqrt{T})T\\, where T denotes the sample size).

- nrep:

  A positive integer. The number of Monte Carlo simulations.

- seed:

  An object specifying if and how the random number generator (rng)
  should be initialized. Either NULL or an integer will be used in a
  call to `set.seed` before simulation. If set, the value is saved as
  "seed" attribute of the returned value. The default, NULL, will not
  change rng state, and return .Random.seed as the "seed" attribute.
  Results are reproducible across the parallel and non-parallel option
  when the same seed is used.

- lag:

  A non-negative integer. Number of lags in the auxiliary regression, as
  in [`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md).

## Value

For `radf_mc_cv` a list that contains the critical values for ADF, BADF,
BSADF and GSADF test statistics. For `radf_mc_distr` a list that
contains the ADF, SADF and GSADF distributions.

## See also

[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
for wild bootstrap critical values and
[`radf_sb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sb_cv.md)
for sieve bootstrap critical values

Other critical values:
[`radf_common_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_common_cv.md),
[`radf_recovery_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery_cv.md),
[`radf_sb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sb_cv.md),
[`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md),
[`radf_sign_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md),
[`radf_sign_dm_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm_cv.md),
[`radf_tt_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md),
[`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md),
[`radf_wb_ps_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_ps_cv.md)

## Examples

``` r
# \donttest{
# Default minimum window
mc <- radf_mc_cv(n = 100)

tidy(mc)
#> # A tibble: 3 × 4
#>   sig       adf  sadf gsadf
#>   <fct>   <dbl> <dbl> <dbl>
#> 1 90    -0.415   1.01  1.60
#> 2 95    -0.0611  1.36  1.90
#> 3 99     0.714   1.85  2.44

# Change the minimum window and the number of simulations
mc2 <- radf_mc_cv(n = 100, nrep = 600, minw = 20)

tidy(mc2)
#> # A tibble: 3 × 4
#>   sig      adf  sadf gsadf
#>   <fct>  <dbl> <dbl> <dbl>
#> 1 90    -0.545  1.00  1.60
#> 2 95    -0.233  1.30  1.90
#> 3 99     0.582  1.73  2.52

mdist <- radf_mc_distr(n = 100, nrep = 1000)

autoplot(mdist)


# Apply the critical values to actual data
rsim_data <- radf(sim_data, minw = 20)
autoplot(rsim_data, cv = mc2)

# }
```
