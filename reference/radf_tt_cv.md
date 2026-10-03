# Monte Carlo critical values for the time-transformed test (STADF/GSTADF)

This is the dedicated critical-value function for
[`radf_tt`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md).
It simulates the asymptotic null distribution of the GLS-demeaned
recursive sup-ADF statistic that
[`radf_tt`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md)
uses. By Theorem 1 of Kurozumi, Skrobotov & Tsarev, this distribution is
free of the volatility process (pivotal). Unlike
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md),
it therefore does not have to be recomputed for each dataset. A large
`n` with the default `nrep` approximates well the T -\> Inf limit used
in the paper.

## Usage

``` r
radf_tt_cv(n, minw = NULL, nrep = 2000L, seed = NULL)
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

## Value

An object of class `radf_cv`/`tt_cv`/`mc_cv` with the same structure as
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md):
the scalars `adf_cv`, `sadf_cv` and `gsadf_cv` for each level plus the
sequences `badf_cv` and `bsadf_cv`. You can use it wherever a `radf_cv`
is accepted.

## Details

You can check the `sadf_cv` column (STADF, that is, `r1 = 0` fixed)
against the asymptotic values of Whitehouse (2019) quoted in footnote 4
of Kurozumi, Skrobotov & Tsarev. For `minw/n = 0.1`, the values at (10\\
1\\ GSTADF (`gsadf_cv`). The paper gives its GSTADF critical values not
as numbers in the text but only as "easily computed from" the code of
the authors in R.

## Note

Since 2026-08-18 the function also computes `badf_cv` and `bsadf_cv`, a
time-varying boundary with one row for each recursion point.
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
and `autoplot` therefore work on
[`radf_tt`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md)
results, and not only [`summary()`](https://rdrr.io/r/base/summary.html)
and `tidy`. The `bsadf_cv` of
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
is a [`cummax()`](https://rdrr.io/r/base/cumsum.html) across replicates,
which works around the output shape of the base C++ engine. Here no such
shortcut is needed, because the `bsadf` of `gls_dfstat_grid()`
(internal) is already the sup over all window starts at each point, and
the boundary is the per-time-point quantile across replicates. We
validated the function in three ways. The last row of `badf_cv` is
bit-identical to `adf_cv`, an exact identity because `adf` is the last
point of `badf` in every replicate. The empirical false-alarm rate under
`H0` is conservative relative to the nominal level (3.3\\ power on a
synthetic bubble matches the established
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md) and
[`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
pipeline almost exactly (18\\ series.
[`radf_sign_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md)
and
[`radf_sign_dm_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm_cv.md)
now compute the same boundary (see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)).

## Status

**\[experimental\]**

## References

Kurozumi, E., Skrobotov, A., & Tsarev, A. (2024). Time-Transformed Test
for Bubbles under Non-stationary Volatility. Journal of Financial
Econometrics.
[doi:10.1093/jjfinec/nbae026](https://doi.org/10.1093/jjfinec/nbae026)

## See also

Other critical values:
[`radf_common_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_common_cv.md),
[`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md),
[`radf_recovery_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery_cv.md),
[`radf_sb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sb_cv.md),
[`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md),
[`radf_sign_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md),
[`radf_sign_dm_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm_cv.md),
[`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md),
[`radf_wb_ps_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_ps_cv.md)

## Examples

``` r
# \donttest{
cv <- radf_tt_cv(n = 200, minw = 20)
tidy(cv)
#> # A tibble: 3 × 4
#>   sig     adf  sadf gsadf
#>   <fct> <dbl> <dbl> <dbl>
#> 1 90    0.899  2.35  3.32
#> 2 95    1.34   2.63  3.67
#> 3 99    1.97   3.27  4.39

# Volatility triples half-way through the sample. This is the case of
# non-stationary volatility that this test is built for, and plain radf()
# over-rejects here
y <- sim_psy1(n = 200, seed = 1, e = sim_vol_break(199))
res <- radf_tt(y, minw = 20)
datestamp(res, cv = cv)
#> 
#> ── Datestamp (min_duration = 0) ───────────────────────── Time-Transformed MC ──
#> 
#> series1 :
#>   Start Peak End Duration   Signal Ongoing
#> 1    21   38  89       68 negative   FALSE
#> 2   148  148 149        1 positive   FALSE
#> 
autoplot(res, cv = cv)

# }
```
