# Common-Bubble Detection via PCA + PSY

`radf_common` tests for a bubble that is common to a panel of series
(Chen, Phillips & Shi, 2023). It extracts the first principal component
of the panel and runs the ordinary
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) test
on it. The output is an ordinary `radf_obj`, so every downstream method
([`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
and so on) works on it without further effort.

## Usage

``` r
radf_common(data, minw = NULL, r = 1)
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

- r:

  Number of principal components to extract (default 1, which the paper
  recommends as "sufficient... for the purpose of bubble
  identification"). Only the first is used for detection. The others are
  returned for inspection in the `"prcomp"` attribute.

## Value

A `radf_obj` (see
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md))
computed on the first principal component of the panel, with the fitted
`prcomp` object attached as an attribute (`attr(x, "prcomp")`).

## Details

Theorem 4.3 of the paper claims that the null limiting distribution of
the resulting statistic is asymptotically identical to the standard
PSY/GSADF distribution, which would let
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
apply directly. An independent validation found that this identity does
**not** hold at practical panel widths `N`. At `N = 100` the true
critical value is more than double that of
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md),
and the gap grows as `N` increases. PCA on a panel of independent
(non-cointegrated) I(1) series does not behave like a single random walk
once there are more series from which transient co-movement can arise.
Use
[`radf_common_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_common_cv.md)
for the critical values and **not**
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md).
The critical values of
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
do not depend on the panel width, and they are badly undersized here
once `N` grows past a handful of series.

## Note

The function returns the output of
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
computed on the extracted factor, and
[`radf_common_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_common_cv.md)
computes the full time-varying boundary alongside the scalar critical
values. The full [`summary()`](https://rdrr.io/r/base/summary.html),
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md),
`tidy` and `autoplot` pipeline therefore works (see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)).

## Status

**\[experimental\]**

## References

Chen, Y., Phillips, P. C. B., & Shi, S. (2023). Common Bubble Detection
in Large Dimensional Financial Systems. Journal of Financial
Econometrics, 21(4), 989-1063.

## See also

[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) for
the underlying test, which is unmodified, and
[`radf_common_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_common_cv.md)
for its critical values, which are specific to the panel width.

Other multivariate:
[`cobubble_test()`](https://kvasilopoulos.github.io/exuber/reference/cobubble_test.md),
[`contagion_reg()`](https://kvasilopoulos.github.io/exuber/reference/contagion_reg.md)

## Examples

``` r
# \donttest{
# A panel of 5 series driven by one shared latent bubble factor
x <- sim_common(n_series = 5, n = 100, seed = 123)
res <- radf_common(x, minw = 20)
print(res)
#> 
#> ── radf (minw = 20, lag = 0) ───────────────────────────────────────────────────
#> 
#>        id     adf  sadf  gsadf
#>   series1  -2.577  5.78  5.922
#> 
#>   gsadf_panel
#>         5.922
#> 

# radf_common_cv() is needed here and radf_mc_cv() does not apply (see Details)
cv <- radf_common_cv(n = 100, N = ncol(x), minw = 20)
summary(res, cv = cv)
#> 
#> ── Summary (minw = 20, lag = 0) ────────────────── Monte Carlo (nboot = 1000) ──
#> 
#> series1 :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf   -2.58 0.333 0.676  1.32
#> 2 sadf   5.78 1.86  2.12   2.71
#> 3 gsadf  5.92 2.26  2.55   3.22
#> 

# The result is an ordinary radf_obj, so autoplot() and datestamp() work directly
autoplot(res, cv = cv)

datestamp(res, cv = cv)
#> 
#> ── Datestamp (min_duration = 0) ───────────────────────────────── Monte Carlo ──
#> 
#> series1 :
#>   Start Peak End Duration   Signal Ongoing
#> 1    46   55  56       10 positive   FALSE
#> 
# }
```
