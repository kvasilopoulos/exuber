# Volatility-Robust Alternatives to radf()

``` r

library(exuber)
```

## The shared problem

Plain
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
assumes a constant innovation variance. Real series rarely have one, and
when volatility varies over time the standard critical values no longer
control the size of the test. exuber offers several fixes, and each
takes a structurally different approach. Four of the five covered below
([`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md),
[`radf_sign_dm()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md),
[`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md)
and
[`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md))
keep the `radf_obj` class and support
[`summary()`](https://rdrr.io/r/base/summary.html),
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html) in
the same way as plain
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md).
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
shows how each plugs into the pipeline and how we validated it.
[`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md)
is the exception. It bundles the statistic of
[`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)
and the classic `supDF` into one union-of-rejections call and has its
own class, not `radf_obj`, so only its own
[`print()`](https://rdrr.io/r/base/print.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
apply. The time-deformation approach of
[`radf_tt()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md)
has its own vignette,
[`vignette("radf-tt")`](https://kvasilopoulos.github.io/exuber/articles/radf-tt.md),
and this one covers the rest.

All of these functions target non-stationary volatility, meaning a
permanent shift or trend in the unconditional innovation variance. They
do not target stationary GARCH-type conditional heteroskedasticity,
whose variance profile is asymptotically flat and leaves plain
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
with the correct size. The running example is therefore the
[`sim_psy1()`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)
bubble driven by
[`sim_vol_break()`](https://kvasilopoulos.github.io/exuber/reference/sim_vol_break.md)
innovations, whose standard deviation triples half-way through the
sample. This is the case in which plain
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
over-rejects the most:

``` r

y <- sim_psy1(n = 200, seed = 1, e = sim_vol_break(199))
```

| Function | Paper | Approach |
|----|----|----|
| [`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md) / [`radf_sign_dm()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md) | Harvey, Leybourne & Zu (2020) | Transforms the series to the cumulated sign of its first differences, which is exactly invariant to any heteroskedasticity pattern and needs no bootstrap. The `_dm` version demeans first, which makes it robust to level shifts (Harvey, Leybourne, Tatlow & Zu 2025). |
| [`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md) | Harvey, Leybourne, Taylor & Zu (2024) | Purges volatility. It divides each first difference by a kernel spot-volatility estimate, cumulates the result and runs the ordinary PSY test on the purged series, whose null distribution is identical to the standard homoskedastic one. |
| [`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md) / [`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md) | Harvey, Leybourne & Zu (2019) | A WLS-weighted recursive Dickey-Fuller statistic (`supBZ`). It weights the observations and does not purge or transform the series. It uses the same kernel spot-volatility estimator as [`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md), but as regression weights. |
| [`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md) | Harvey, Leybourne & Zu (2019) | Combines `supDF` (the classic statistic) and `supBZ` (the statistic of [`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)) as a union of rejections, using a wild bootstrap sized jointly for both. It catches whichever of the two has power on a given series. |

## Sign-based: `radf_sign()`

Since 2026-08-18
[`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md)
also has full pipeline support.
[`radf_sign_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md)
now computes the time-varying `badf_cv` and `bsadf_cv` boundary and not
only the scalar critical values that
[`summary()`](https://rdrr.io/r/base/summary.html) needs (see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the validation):

``` r

res <- radf_sign(y, minw = 20)
cv <- radf_sign_cv(n = 200, minw = 20)
summary(res, cv = cv)
#> 
#> ── Summary (minw = 20, lag = 0) ──────────────── Sign-Based MC (nboot = 2000) ──
#> 
#> series1 :
#> # A tibble: 3 × 5
#>   stat   tstat  `90`  `95`  `99`
#>   <fct>  <dbl> <dbl> <dbl> <dbl>
#> 1 adf   -0.293 0.855  1.32  2.06
#> 2 sadf   4.47  2.34   2.70  3.43
#> 3 gsadf  8.88  3.51   3.91  4.92
datestamp(res, cv = cv)
#> 
#> ── Datestamp (min_duration = 0) ─────────────────────────────── Sign-Based MC ──
#> 
#> series1 :
#>   Start Peak End Duration   Signal Ongoing
#> 1    84   84  85        1 positive   FALSE
#> 2    87  103 122       35 positive   FALSE
#> 3   123  123 124        1 positive   FALSE
```

## Kernel-purged: `radf_kp()`

[`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md)
purges volatility and then calls
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
unmodified, so it has full pipeline support in the simplest possible
way. It needs no new critical-value code, and
[`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
applies as it is:

``` r

res_kp <- radf_kp(y, minw = 20)
cv_kp <- radf_mc_cv(n = attr(res_kp, "n"), minw = 20)
summary(res_kp, cv = cv_kp)
#> 
#> ── Summary (minw = 20, lag = 0) ────────────────── Monte Carlo (nboot = 1000) ──
#> 
#> series1 :
#> # A tibble: 3 × 5
#>   stat  tstat   `90`   `95`  `99`
#>   <fct> <dbl>  <dbl>  <dbl> <dbl>
#> 1 adf   -1.72 -0.470 -0.140 0.535
#> 2 sadf   1.50  1.07   1.37  1.90 
#> 3 gsadf  2.63  1.98   2.25  2.80
```

## WLS + kernel volatility: `radf_sbz()`

Since 2026-08-22
[`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)
is a statistic function of its own, separate from the union test.
[`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md)
computes the time-varying `badf_cv` and `bsadf_cv` boundary in the same
way as
[`radf_tt_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md)
and
[`radf_sign_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md)
(see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the validation), so it has full pipeline support:

``` r

res_sbz <- radf_sbz(y, minw = 20)
cv_sbz <- radf_sbz_cv(y, minw = 20, nboot = 200, seed = 1)
summary(res_sbz, cv = cv_sbz)
#> 
#> ── Summary (minw = 20, lag = 0) ────────── Wild Bootstrap (SBZ) (nboot = 200) ──
#> 
#> series1 :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf   -1.39 0.825  1.13  1.61
#> 2 sadf   1.67 1.58   2.11  3.36
#> 3 gsadf  1.91 3.37   5.06  6.21
```

The kernel-volatility weighting that makes `supBZ` robust to
heteroskedasticity costs some power relative to the other tests. The
default bubble of
[`sim_psy1()`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)
is mild (30 periods at `rho = 1 + 200^-0.6`, then a collapse). It does
not clear the 95% critical value of `supBZ` here, although every other
test above rejects on the same series. A stronger bubble that does not
collapse (`rho = 1.03` from `t = 120` to the end of the sample, on the
same volatility break) does clear it:

``` r

y_strong <- sim_psy1(n = 200, te = 120, tf = 200, c = 0.03, alpha = 0, seed = 1,
                     e = sim_vol_break(199))
res_sbz2 <- radf_sbz(y_strong, minw = 20)
cv_sbz2 <- radf_sbz_cv(y_strong, minw = 20, nboot = 200, seed = 1)
summary(res_sbz2, cv = cv_sbz2)
#> 
#> ── Summary (minw = 20, lag = 0) ────────── Wild Bootstrap (SBZ) (nboot = 200) ──
#> 
#> series1 :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf    4.83 0.948  1.65  2.65
#> 2 sadf   4.83 2.24   2.49  3.26
#> 3 gsadf  5.29 2.77   3.00  3.58
datestamp(res_sbz2, cv = cv_sbz2)
#> 
#> ── Datestamp (min_duration = 0) ──────────────────────── Wild Bootstrap (SBZ) ──
#> 
#> series1 :
#>   Start Peak End Duration   Signal Ongoing
#> 1   129  129 130        1 positive   FALSE
#> 2   132  132 133        1 positive   FALSE
#> 3   134  134 135        1 positive   FALSE
#> 4   172  200 200       29 positive    TRUE
```

This is the same trade-off that
[`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md),
below, hedges against by combining `supBZ` with the classic `supDF`.

## Union-of-rejections: `radf_sbz_union()`

``` r

radf_sbz_union(y, nboot = 200, seed = 1)
#> 
#> ── radf_sbz_union (minw = 27, nboot = 200) ─────────────────────────────────────
#> 
#>    series  supDF  supBZ      U  p_supDF  p_supBZ    p_U
#>   series1  9.329   1.67  9.329        0    0.085  0.005
```

`supDF` is the classic PWY statistic, and `supBZ` is the WLS-weighted
version that
[`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)
also returns on its own. `U` is their union. Each statistic has its own
bootstrap p-value, so one can flag a series without the other. Here
`supDF` rejects, `supBZ` does not, and the union `U` follows `supDF`.
The value of `U` is defined with a bootstrap-derived scaling ratio
between `supDF` and `supBZ`, and its size guarantee requires that the
`supDF` and `supBZ` bootstrap draws come from the same resampled series
in each replicate. For both reasons
[`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md)
cannot be reconstructed by calling
[`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md)
and plain
[`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
separately. It stays a single bundled call with its own class and is not
a `radf_obj`.

## Which to reach for

- If you want exact invariance to any heteroskedasticity pattern with no
  bootstrap, use
  [`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md).
  Use
  [`radf_sign_dm()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md)
  if a level shift, and not only volatility, is a concern. Both have
  full pipeline support.
- If you want to stay closest to plain
  [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
  with no new critical-value code, use
  [`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md).
- If you want the efficiency gain from WLS together with full
  [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
  and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  support, use
  [`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)
  with
  [`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md).
- If you want to hedge between the classic and the WLS-weighted
  statistics on the same series and do not need
  [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
  or
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
  use
  [`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md).
  It is the only function in this group without pipeline support,
  because `U` bundles both statistics and their scalar joint critical
  value in one call and does not return a `radf_obj`.
- If volatility is the main concern and you prefer a time-deformation
  approach without a bootstrap, use
  [`radf_tt()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md)
  (see
  [`vignette("radf-tt")`](https://kvasilopoulos.github.io/exuber/articles/radf-tt.md)).
- If the volatility is unknown or complex and a bootstrap is acceptable,
  [`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
  remains the general-purpose choice.
