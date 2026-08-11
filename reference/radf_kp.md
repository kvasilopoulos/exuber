# Kernel-Purged Heteroskedasticity-Robust PSY Test

`radf_kp` implements the bootstrap-free heteroskedasticity-robust PSY
test of Harvey, Leybourne, Taylor & Zu (2024): it "purges" unconditional
heteroskedasticity by cumulating the series' first differences after
dividing each by a kernel spot-volatility estimate (eq. 4-5), then runs
the ordinary (with-intercept)
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) on
the purged series.

## Usage

``` r
radf_kp(data, minw = NULL, kernel = c("gaussian", "uniform"), h = NULL)
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

- kernel:

  Kernel for the spot-volatility estimator, `"gaussian"` (default, as in
  the paper) or `"uniform"`.

- h:

  Bandwidth for the spot-volatility estimator. Default
  `0.1 * T^(-0.25)`, the paper's own setting (Table I, Section 6).

## Value

A `radf_obj`, identical in structure to
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s
output (so
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) etc. all
apply directly), computed on the volatility-purged series.

## Details

Because the purged statistic's null limiting distribution is proven
(Theorem 1 / Remark 3.2) to be identical to the standard homoskedastic
GSADF null,
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
– exuber's existing, already-fast Monte Carlo critical values – applies
directly to the result; no new bootstrap or simulation machinery is
needed, unlike
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
or
[`radf_sbz_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md).

Only the with-intercept variant (\\PSY\_\sigma\\ in the paper) is
implemented. The paper also proposes a without-intercept variant and a
union-of-rejections test combining both; these are not implemented here
(see the package's enhancement notes for the cost/benefit reasoning).

## Status

**\[experimental\]**

## References

Harvey, D. I., Leybourne, S. J., Taylor, A. M. R., & Zu, Y. (2024). A
new heteroskedasticity-robust test for explosive bubbles. Journal of
Time Series Analysis.
[doi:10.1111/jtsa.12784](https://doi.org/10.1111/jtsa.12784)

## See also

[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
for this test's (unmodified) critical values,
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
for a bootstrap-based alternative, and
[`radf_tt`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md)
for another bootstrap-free alternative.
