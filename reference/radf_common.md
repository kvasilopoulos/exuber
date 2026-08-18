# Common-Bubble Detection via PCA + PSY

`radf_common` tests for a bubble common to a panel of series (Chen,
Phillips & Shi, 2023): it extracts the panel's first principal component
and runs the ordinary
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) test
on it – and every downstream method
([`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md),
...) works on it for free, since the output is an ordinary `radf_obj`.

## Usage

``` r
radf_common(data, minw = NULL, r = 1)
```

## Arguments

- data:

  A univariate or multivariate numeric time series object, a numeric
  vector or matrix, or a data.frame. A column may have leading and/or
  trailing `NA` values (an uneven/unbalanced panel where series enter or
  exit the sample at different times) – those periods are filled with
  `NA` in `badf`/`bsadf` and excluded from that series' `adf`/`sadf`/
  `gsadf`. Interior `NA` values (a gap in the middle of a series) are
  not supported. When any series is padded this way, the panel statistic
  (`bsadf_panel`/`gsadf_panel`) is not available and is returned as
  `NA`, with a warning.

- minw:

  A positive integer. The minimum window size (default = \\(0.01 +
  1.8/\sqrt(T))T\\, where T denotes the sample size).

- r:

  Number of principal components to extract (default 1, the paper's own
  recommendation: "sufficient... for the purpose of bubble
  identification"). Only the first is used for detection; the rest are
  returned for inspection via the `"prcomp"` attribute.

## Value

A `radf_obj` (see
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md))
computed on the panel's first principal component, with the fitted
`prcomp` object attached as an attribute (`attr(x, "prcomp")`).

## Details

The paper's own Theorem 4.3 claims the resulting statistic's null
limiting distribution is asymptotically identical to the standard
PSY/GSADF one, which would let
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
apply directly. An independent validation found this identity does
**not** hold at practical panel widths `N`: the true critical value is
more than double
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)'s
at `N = 100`, and the gap grows as `N` increases – PCA on a panel of
merely independent (non-cointegrated) I(1) series does not behave like a
single random walk once there are more series to draw transient
co-movement from. Use
[`radf_common_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_common_cv.md)
for critical values, **not**
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md),
which has no dependence on panel width and is badly undersized here once
`N` grows past a handful of series.

## Note

Returns
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s
own output (computed on the extracted factor), and
[`radf_common_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_common_cv.md)
computes the full time-varying boundary alongside the scalar critical
values, so the full
[`summary()`](https://rdrr.io/r/base/summary.html)/[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/`tidy`/`autoplot`
pipeline works – see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md).

## Status

**\[experimental\]**

## References

Chen, Y., Phillips, P. C. B., & Shi, S. (2023). Common Bubble Detection
in Large Dimensional Financial Systems. Journal of Financial
Econometrics, 21(4), 989-1063.

## See also

[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) for
the underlying (unmodified) test, and
[`radf_common_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_common_cv.md)
for its (panel-width-specific) critical values.

## Examples

``` r
# \donttest{
res <- radf_common(sim_data, minw = 20)
print(res)
#> 
#> ── radf (minw = 20, lag = 0) ───────────────────────────────────────────────────
#> 
#>        id     adf   sadf  gsadf
#>   series1  -2.734  7.145  7.145
#> 
#>   gsadf_panel
#>         7.145
#> 

# radf_common_cv() is needed here -- NOT radf_mc_cv(), see Details
cv <- radf_common_cv(n = 100, N = ncol(sim_data), minw = 20)
summary(res, cv = cv)
#> 
#> ── Summary (minw = 20, lag = 0) ────────────────── Monte Carlo (nboot = 1000) ──
#> 
#> series1 :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf   -2.73 0.519 0.875  1.57
#> 2 sadf   7.15 1.89  2.25   2.80
#> 3 gsadf  7.15 2.38  2.68   3.34
#> 
# }
```
