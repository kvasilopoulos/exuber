# Common-Bubble Detection via PCA + PSY

`radf_common` tests for a bubble common to a panel of series (Chen,
Phillips & Shi, 2023): it extracts the panel's first principal component
and runs the ordinary
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) test
on it. Per the paper's Theorem 4.3, the resulting statistic's null
limiting distribution is identical to the standard PSY/GSADF one, so
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
(or
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md),
for heteroskedasticity robustness) applies directly to the result with
no modification – and every downstream method
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

## Status

**\[experimental\]**

## References

Chen, Y., Phillips, P. C. B., & Shi, S. (2023). Common Bubble Detection
in Large Dimensional Financial Systems. Journal of Financial
Econometrics, 21(4), 989-1063.

## See also

[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) for
the underlying (unmodified) test, and
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
for its critical values.
