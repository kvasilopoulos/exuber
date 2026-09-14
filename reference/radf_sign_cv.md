# Monte Carlo Critical Values for the Sign-Based Test

Simulates the asymptotic null distribution of
[`radf_sign`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md)'s
statistic. Per Theorem 2 of Harvey, Leybourne & Zu (2020), this
distribution does not depend on the volatility process at all (exact
invariance) – so, like
[`radf_tt_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md)
and unlike
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md),
it does not need to be recomputed per dataset: a large `n` with the
default `nrep` approximates the paper's own `T -> Inf` limit.

## Usage

``` r
radf_sign_cv(n, minw = NULL, nrep = 2000L, seed = NULL)
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

## Value

An object of class `radf_cv`/`sign_cv`/`mc_cv` with the same structure
as
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md).

## Details

`sadf_cv` (single-supremum, `r1 = 0` fixed) can be checked against the
paper's Table 1 asymptotic (`T = Inf`) sPWY values: for `minw/n = 0.1`,
(10\\ `gsadf_cv` (double-supremum) corresponds to the sPSY row: (2.933,
3.180, 3.655).

## Status

**\[experimental\]**

## References

Harvey, D. I., Leybourne, S. J., & Zu, Y. (2020). Sign-based unit root
tests for explosive financial bubbles in the presence of
deterministically time-varying volatility. Econometric Theory, 36(1),
122-169.

## See also

Other critical values:
[`radf_common_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_common_cv.md),
[`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md),
[`radf_recovery_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery_cv.md),
[`radf_sb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sb_cv.md),
[`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md),
[`radf_sign_dm_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm_cv.md),
[`radf_tt_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md),
[`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md),
[`radf_wb_ps_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_ps_cv.md)

## Examples

``` r
# \donttest{
cv <- radf_sign_cv(n = 200, minw = 20)
tidy(cv)
#> # A tibble: 3 × 4
#>   sig     adf  sadf gsadf
#>   <fct> <dbl> <dbl> <dbl>
#> 1 90    0.926  2.43  3.48
#> 2 95    1.32   2.79  3.91
#> 3 99    2.05   3.30  4.93
# }
```
