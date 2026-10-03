# SBZ Weighted Least Squares Bubble Test with Union-of-Rejections

`radf_sbz_union` performs the HLST (2016) wild bootstrap, the same
algorithm as
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md),
*jointly* on the classic sup-ADF statistic (`supDF`, that is, the `sadf`
of [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md))
and on the WLS/kernel-volatility statistic `supBZ` of Harvey, Leybourne
& Zu (2019). It combines them into the union-of-rejections statistic `U`
of the paper. `supBZ` can have substantially higher power than `supDF`
under many patterns of time-varying volatility, and lower power under
others, for example upward volatility trends. `U` is designed to capture
whichever of the two is more powerful for a given series.

## Usage

``` r
radf_sbz_union(
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

  Kernel for the spot-volatility estimator (eq. 6), `"gaussian"`
  (default, as in the paper) or `"uniform"`.

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

A list with bootstrap p-values (`p_supDF`, `p_supBZ`, `p_U`) and
critical values (`supDF_cv`, `supBZ_cv`, `U_cv`) for each series.

## Details

The value of `U`, and not only its significance, is defined with a
bootstrap-calibrated scaling ratio between the 95\\ `supDF` and `supBZ`
(Section 2.3 of the paper). The union also keeps its size guarantee
(Theorem 3 of the paper) only if the joint bootstrap computes `supDF`
and `supBZ` from the *same* resampled series in each replication. This
coupling is why the function stays a single bundled function, and does
not split into a statistic and a critical-value function as most of
exuber does. `supBZ` alone has no such coupling, so it does split. See
[`radf_sbz`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)
and
[`radf_sbz_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md)
for the route that uses only `supBZ`, with the usual
[`summary()`](https://rdrr.io/r/base/summary.html),
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md),
`tidy` and `autoplot` pipeline.

## Note

This function bundles the statistic and its critical values in a single
call. Unlike
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md) and
[`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md),
there is no separate statistic function without critical values and no
other critical-value function to pair it with, because the value of `U`
requires the bootstrap by construction (see Details).

The function returns its own class and not `radf_obj`, so it does not
work with [`summary()`](https://rdrr.io/r/base/summary.html),
`\link{datestamp}` and `tidy`. It has its own
[`print()`](https://rdrr.io/r/base/print.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods instead. [`print()`](https://rdrr.io/r/base/print.html) shows
the test statistics together with their critical values, because the
object bundles both. The
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
method compares `supDF`, `supBZ` and `U` with their critical values for
each series. See
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for which functions fit the shared pipeline and which do not.

## Status

**\[experimental\]**

## References

Harvey, D. I., Leybourne, S. J., & Zu, Y. (2019). Testing explosive
bubbles with time-varying volatility. Econometric Reviews, 38(10),
1131-1151.

## See also

[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
for the underlying wild bootstrap, which uses `supDF` only,
[`radf_sbz`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)
and
[`radf_sbz_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md)
for the route that uses `supBZ` only and has full pipeline support, and
[`radf_tt`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md)
for a heteroskedasticity-robust alternative that needs no bootstrap.

Other volatility-robust tests:
[`cusum_test()`](https://kvasilopoulos.github.io/exuber/reference/cusum_test.md),
[`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md),
[`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md),
[`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md),
[`radf_sign_dm()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md),
[`radf_tt()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md),
[`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md)

## Examples

``` r
# \donttest{
y <- sim_psy1(n = 200, te = 120, tf = 200, c = 0.03, alpha = 0, seed = 1,
  e = sim_vol_break(199))
res <- radf_sbz_union(y, nboot = 200, seed = 1)
print(res)
#> 
#> ── radf_sbz_union (minw = 27, nboot = 200) ─────────────────────────────────────
#> 
#>    series  supDF  supBZ      U  p_supDF  p_supBZ    p_U
#>   series1  9.264  4.829  9.264        0    0.005  0.005
#> 
autoplot(res)

# }
```
