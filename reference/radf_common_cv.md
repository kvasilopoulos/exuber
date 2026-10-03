# Critical Values for the Common-Bubble (PCA + PSY) Test

`radf_common_cv` simulates critical values for
[`radf_common`](https://kvasilopoulos.github.io/exuber/reference/radf_common.md)
under its own null of no common explosive factor. The null is a panel of
`N` *independent* random walks, from which one principal component is
extracted and tested exactly as in
[`radf_common`](https://kvasilopoulos.github.io/exuber/reference/radf_common.md).
Independent validation showed that
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md),
whose critical values do not depend on the panel width, is badly
undersized as a stand-in for this null once `N` grows past a handful of
series. The null distribution here does depend on `N`, so `N` must match
the panel on which
[`radf_common`](https://kvasilopoulos.github.io/exuber/reference/radf_common.md)
was run.

## Usage

``` r
radf_common_cv(n, N, minw = NULL, nrep = 1000L, seed = NULL)
```

## Arguments

- n:

  A positive integer. The sample size (number of time periods).

- N:

  A positive integer, at least 2. The panel width (number of series) on
  which
  [`radf_common`](https://kvasilopoulos.github.io/exuber/reference/radf_common.md)
  will be run. The critical value depends on it, unlike that of
  [`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md).

- minw:

  A positive integer. The minimum window size (default = \\(0.01 +
  1.8/\sqrt{T})T\\, where T denotes the sample size).

- nrep:

  A positive integer. The number of Monte Carlo simulations.

- seed:

  An object specifying if and how the random number generator (rng)
  should be initialized. It is either NULL or an integer, which is
  passed to `set.seed` before the simulation. If you set it, the value
  is saved as the "seed" attribute of the returned value. The default,
  NULL, leaves the state of the rng unchanged and returns .Random.seed
  as the "seed" attribute. Results are reproducible across the parallel
  and the non-parallel option when you use the same seed.

## Value

A list with `adf_cv`, `sadf_cv`, `gsadf_cv`, `badf_cv` and `bsadf_cv`.
It has the same shape as the return value of
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md),
so you can use it as the `cv` argument of
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md),
`tidy` and `autoplot` on a
[`radf_common`](https://kvasilopoulos.github.io/exuber/reference/radf_common.md)
result.

## Status

**\[experimental\]**

## References

Chen, Y., Phillips, P. C. B., & Shi, S. (2023). Common Bubble Detection
in Large Dimensional Financial Systems. Journal of Financial
Econometrics, 21(4), 989-1063.

## See also

[`radf_common`](https://kvasilopoulos.github.io/exuber/reference/radf_common.md),
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)

Other critical values:
[`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md),
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
cv <- radf_common_cv(n = 100, N = 5, minw = 20, nrep = 200)
tidy(cv)
#> # A tibble: 3 × 4
#>   sig     adf  sadf gsadf
#>   <fct> <dbl> <dbl> <dbl>
#> 1 90    0.442  1.79  2.26
#> 2 95    0.766  1.98  2.47
#> 3 99    1.32   2.40  3.22
# }
```
