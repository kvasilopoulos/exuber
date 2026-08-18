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

## Note

Returns
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s own
output unmodified, so the full
[`summary()`](https://rdrr.io/r/base/summary.html)/[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/`tidy`/`autoplot`
pipeline works exactly as it does for plain
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md) –
see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md).

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

## Examples

``` r
# \donttest{
res <- radf_kp(sim_data, minw = 20)
print(res)
#> 
#> ── radf (minw = 20, lag = 0) ───────────────────────────────────────────────────
#> 
#>      id      adf     sadf   gsadf
#>    psy1  -0.4387   0.1754  1.6743
#>    psy2  -2.4026   0.9183  2.0255
#>   evans  -1.9339  -0.7140  0.4889
#>     div  -2.3185   0.5835  0.8997
#>    blan  -2.4701  -1.4602  0.4523
#> 
#>   gsadf_panel
#>     -0.002803
#> 

# radf_mc_cv() applies unmodified -- see Details
cv <- radf_mc_cv(n = attr(res, "n"), minw = 20)
summary(res, cv = cv)
#> 
#> ── Summary (minw = 20, lag = 0) ────────────────── Monte Carlo (nboot = 1000) ──
#> 
#> psy1 :
#> # A tibble: 3 × 5
#>   stat   tstat   `90`   `95`  `99`
#>   <fct>  <dbl>  <dbl>  <dbl> <dbl>
#> 1 adf   -0.439 -0.488 -0.150 0.408
#> 2 sadf   0.175  0.910  1.26  1.99 
#> 3 gsadf  1.67   1.62   1.90  2.47 
#> 
#> psy2 :
#> # A tibble: 3 × 5
#>   stat   tstat   `90`   `95`  `99`
#>   <fct>  <dbl>  <dbl>  <dbl> <dbl>
#> 1 adf   -2.40  -0.488 -0.150 0.408
#> 2 sadf   0.918  0.910  1.26  1.99 
#> 3 gsadf  2.03   1.62   1.90  2.47 
#> 
#> evans :
#> # A tibble: 3 × 5
#>   stat   tstat   `90`   `95`  `99`
#>   <fct>  <dbl>  <dbl>  <dbl> <dbl>
#> 1 adf   -1.93  -0.488 -0.150 0.408
#> 2 sadf  -0.714  0.910  1.26  1.99 
#> 3 gsadf  0.489  1.62   1.90  2.47 
#> 
#> div :
#> # A tibble: 3 × 5
#>   stat   tstat   `90`   `95`  `99`
#>   <fct>  <dbl>  <dbl>  <dbl> <dbl>
#> 1 adf   -2.32  -0.488 -0.150 0.408
#> 2 sadf   0.584  0.910  1.26  1.99 
#> 3 gsadf  0.900  1.62   1.90  2.47 
#> 
#> blan :
#> # A tibble: 3 × 5
#>   stat   tstat   `90`   `95`  `99`
#>   <fct>  <dbl>  <dbl>  <dbl> <dbl>
#> 1 adf   -2.47  -0.488 -0.150 0.408
#> 2 sadf  -1.46   0.910  1.26  1.99 
#> 3 gsadf  0.452  1.62   1.90  2.47 
#> 
# }
```
