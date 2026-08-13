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
  1.8/\sqrt(T))T\\, where T denotes the sample size).

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

## Examples

``` r
# \donttest{
# Default minimum window
mc <- radf_mc_cv(n = 100)

tidy(mc)
#> # A tibble: 3 × 4
#>   sig      adf  sadf gsadf
#>   <fct>  <dbl> <dbl> <dbl>
#> 1 90    -0.520 0.977  1.63
#> 2 95    -0.238 1.25   1.97
#> 3 99     0.590 1.77   2.67

# Change the minimum window and the number of simulations
mc2 <- radf_mc_cv(n = 100, nrep = 600, minw = 20)

tidy(mc2)
#> # A tibble: 3 × 4
#>   sig      adf  sadf gsadf
#>   <fct>  <dbl> <dbl> <dbl>
#> 1 90    -0.447 0.941  1.62
#> 2 95    -0.173 1.25   1.90
#> 3 99     0.516 1.88   2.34

mdist <- radf_mc_distr(n = 100, nrep = 1000)

autoplot(mdist)

# }
```
