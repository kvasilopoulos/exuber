# Monte Carlo Critical Values for Reverse-Regression Recovery Dating

Computes critical values for the reverse-regression BSADF statistic that
[`radf_recovery`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md)
uses. They are calibrated to the null limiting distribution of that
statistic (Theorem 1 of Phillips & Shi 2014) and not to the standard
forward boundary of
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md).
The simulated null path is reversed before the recursive computation,
because reversal induces an endogeneity that has no analogue in the
forward regression (see the Details of
[`radf_recovery`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md)).

## Usage

``` r
radf_recovery_cv(n, minw = NULL, nrep = 1000L, seed = NULL, lag = 0)
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
  should be initialized. It is either NULL or an integer, which is
  passed to `set.seed` before the simulation. If you set it, the value
  is saved as the "seed" attribute of the returned value. The default,
  NULL, leaves the state of the rng unchanged and returns .Random.seed
  as the "seed" attribute. Results are reproducible across the parallel
  and the non-parallel option when you use the same seed.

- lag:

  A non-negative integer. Number of lags in the auxiliary regression, as
  in [`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md).

## Value

A list of class `radf_cv` with a single element, `bsadf_cv`. It is a
matrix of critical values (columns `90%`, `95%`, `99%`) with one row for
each reverse-time position, aligned in the same way as the `bsadf_cv` of
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
is aligned to `radf()$bsadf`.

## Note

[`print()`](https://rdrr.io/r/base/print.html) and
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) are not yet
implemented for the class of this object. `recovery_cv` has no
`tidy_radf_cv` method. The objects from
[`radf_sign_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md)
and
[`radf_tt_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md)
fall back to the method of `mc_cv`, but that fallback does not apply
here, because this object carries only `bsadf_cv` and not the `adf_cv`,
`sadf_cv` and `gsadf_cv` fields that the method expects. Inspect
`cv$bsadf_cv` directly. Calling `print(cv)` or `tidy(cv)` currently
gives an error.

## Status

**\[experimental\]**

## See also

[`radf_recovery`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md),
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)

Other critical values:
[`radf_common_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_common_cv.md),
[`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md),
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
cv <- radf_recovery_cv(n = 100, minw = 20, nrep = 200)
range(cv$bsadf_cv)
#> [1] -0.3351885  1.9060722
# }
```
