# Wild Bootstrap Critical Values for the SBZ Statistic

`radf_sbz_cv` performs the HLST (2016) wild bootstrap. It is the same
algorithm as
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md),
applied to the WLS/kernel-volatility statistic of
[`radf_sbz`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)
instead of the classic `supDF` statistic. It generates critical values
that include the time-varying `badf_cv` and `bsadf_cv` boundary that
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
and `autoplot` need, and not only the three scalar critical values that
[`summary()`](https://rdrr.io/r/base/summary.html) uses.

## Usage

``` r
radf_sbz_cv(
  data,
  minw = NULL,
  nboot = 499L,
  kernel = c("gaussian", "uniform"),
  h = NULL,
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

- nboot:

  A positive integer. Number of bootstraps (default = 500L).

- kernel:

  Kernel for the spot-volatility estimator (eq. 6 of Harvey, Leybourne &
  Zu 2019), `"gaussian"` (default, as in the paper) or `"uniform"`.

- h:

  Bandwidth for the spot-volatility estimator. The default is
  leave-one-out cross-validation over the search range of the paper.

- seed:

  An object specifying if and how the random number generator (rng)
  should be initialized. It is either NULL or an integer, which is
  passed to `set.seed` before the simulation. If you set it, the value
  is saved as the "seed" attribute of the returned value. The default,
  NULL, leaves the state of the rng unchanged and returns .Random.seed
  as the "seed" attribute. Results are reproducible across the parallel
  and the non-parallel option when you use the same seed.

## Value

An object of class `radf_cv`/`sbz_cv`/`wb_cv`: a list with the critical
values `adf_cv`, `sadf_cv` and `gsadf_cv` (one row per series) and
`badf_cv` and `bsadf_cv` (one array per series, one row per recursion
point).

## Status

**\[experimental\]**

## References

Harvey, D. I., Leybourne, S. J., & Zu, Y. (2019). Testing explosive
bubbles with time-varying volatility. Econometric Reviews, 38(10),
1131-1151.

## See also

[`radf_sbz`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)
for the statistic that this function pairs with, and
[`radf_sbz_union`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md)
for the bundled union-of-rejections test against the classic `supDF`
statistic. You cannot obtain that test from this function and
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
separately (see the Details of
[`radf_sbz_union`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md)
for why).

Other critical values:
[`radf_common_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_common_cv.md),
[`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md),
[`radf_recovery_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery_cv.md),
[`radf_sb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sb_cv.md),
[`radf_sign_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md),
[`radf_sign_dm_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm_cv.md),
[`radf_tt_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md),
[`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md),
[`radf_wb_ps_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_ps_cv.md)

## Examples

``` r
# \donttest{
y <- sim_psy1(150, seed = 1)
res <- radf_sbz(y, minw = 20)
cv <- radf_sbz_cv(y, minw = 20, nboot = 200, seed = 1)
summary(res, cv = cv)
#> 
#> ── Summary (minw = 20, lag = 0) ────────── Wild Bootstrap (SBZ) (nboot = 200) ──
#> 
#> series1 :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf   0.103  1.14  1.56  1.81
#> 2 sadf  3.95   2.13  2.48  3.24
#> 3 gsadf 4.49   2.60  2.92  3.50
#> 
datestamp(res, cv = cv)
#> 
#> ── Datestamp (min_duration = 0) ──────────────────────── Wild Bootstrap (SBZ) ──
#> 
#> series1 :
#>   Start Peak End Duration   Signal Ongoing
#> 1    64   82 102       38 positive   FALSE
#> 2   109  111 113        4 positive   FALSE
#> 3   114  114 115        1 positive   FALSE
#> 
# }
```
