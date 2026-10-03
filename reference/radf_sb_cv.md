# Panel Sieve Bootstrap Critical Values

`radf_sb_cv` computes critical values for the panel recursive unit root
test with the sieve bootstrap procedure of Pavlidis et al. (2016).
`radf_sb_distr` computes the distribution.

## Usage

``` r
radf_sb_cv(
  data,
  minw = NULL,
  lag = 0L,
  nboot = 500L,
  type = c("fixed", "aic", "bic"),
  max_lag = 8L,
  seed = NULL
)

radf_sb_distr(
  data,
  minw = NULL,
  lag = 0L,
  nboot = 500L,
  type = c("fixed", "aic", "bic"),
  max_lag = 8L,
  seed = NULL
)
```

## Arguments

- data:

  A univariate or multivariate numeric time series object, a numeric
  vector or matrix, or a data.frame. A column may have leading or
  trailing `NA` values, which describes an unbalanced panel in which
  series enter or exit the sample at different times. Those periods are
  filled with `NA` in `badf` and `bsadf` and excluded from the `adf`,
  `sadf` and `gsadf` of that series. Interior `NA` values (a gap in the
  middle of a series) are not supported. When any series is padded in
  this way, the panel statistics (`bsadf_panel` and `gsadf_panel`) are
  not available, and the function returns `NA` for them with a warning.

- minw:

  A positive integer. The minimum window size (default = \\(0.01 +
  1.8/\sqrt{T})T\\, where T denotes the sample size).

- lag:

  A non-negative integer. The lag length of the Augmented Dickey-Fuller
  regression (default = 0L).

- nboot:

  A positive integer. Number of bootstraps (default = 500L).

- type:

  Lag-order selection. `"fixed"` (default) uses `lag` as given, as in
  the single-`lag` behavior of
  [`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md).
  `"aic"` and `"bic"` select the lag automatically for each series with
  `lag_select()` (internal), and the function takes the maximum across
  the panel because the rest of it assumes one common lag order. This is
  the fix of Pedersen & Schütte (2020) for the size distortion that a
  fixed lag causes under autocorrelated innovations.

- max_lag:

  Maximum lag order to search over when `type` is `"aic"` or `"bic"`. It
  is ignored when `type = "fixed"`.

- seed:

  An object specifying if and how the random number generator (rng)
  should be initialized. It is either NULL or an integer, which is
  passed to `set.seed` before the simulation. If you set it, the value
  is saved as the "seed" attribute of the returned value. The default,
  NULL, leaves the state of the rng unchanged and returns .Random.seed
  as the "seed" attribute. Results are reproducible across the parallel
  and the non-parallel option when you use the same seed.

## Value

For `radf_sb_cv`, a list with the critical values for the panel BSADF
and panel GSADF test statistics. For `radf_sb_distr`, a numeric vector
with the distribution of the panel GSADF statistic.

## References

Pavlidis, E., Yusupova, A., Paya, I., Peel, D., Martínez-García, E.,
Mack, A., & Grossman, V. (2016). Episodes of exuberance in housing
markets: In search of the smoking gun. The Journal of Real Estate
Finance and Economics, 53(4), 419-449.
[doi:10.1007/s11146-015-9531-2](https://doi.org/10.1007/s11146-015-9531-2)

Pedersen, T. Q., & Schütte, E. C. M. (2020). Testing for explosive
bubbles in the presence of autocorrelated innovations. Journal of
Empirical Finance, 58, 207-225.

## See also

[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
for Monte Carlo critical values and
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
for wild Bootstrap critical values

Other critical values:
[`radf_common_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_common_cv.md),
[`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md),
[`radf_recovery_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery_cv.md),
[`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md),
[`radf_sign_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md),
[`radf_sign_dm_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm_cv.md),
[`radf_tt_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md),
[`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md),
[`radf_wb_ps_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_ps_cv.md)

## Examples

``` r
# \donttest{

rsim_data <- radf(sim_data, lag = 1)

# Critical values should have the same lag length as \code{radf()}
sb <- radf_sb_cv(sim_data, lag = 1)

tidy(sb)
#> # A tibble: 3 × 3
#>   id    sig   gsadf_panel
#>   <fct> <fct>       <dbl>
#> 1 panel 90          0.342
#> 2 panel 95          0.486
#> 3 panel 99          0.770

summary(rsim_data, cv = sb)
#> 
#> ── Summary (minw = 19, lag = 1) ─────────────── Sieve Bootstrap (nboot = 500) ──
#> 
#> panel :
#> # A tibble: 1 × 5
#>   stat        tstat  `90`  `95`  `99`
#>   <fct>       <dbl> <dbl> <dbl> <dbl>
#> 1 gsadf_panel  1.89 0.342 0.486 0.770
#> 

autoplot(rsim_data, cv = sb)


# Simulate distribution
sdist <- radf_sb_distr(sim_data, lag = 1, nboot = 1000)

autoplot(sdist)


# Automatic BIC lag selection instead of a fixed lag
sb_bic <- radf_sb_cv(sim_data, type = "bic")
# }
```
