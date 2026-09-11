# Volatility-Robust Alternatives to radf()

``` r

library(exuber)
```

## The shared problem

Plain
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
assumes constant innovation variance. Real series rarely have that, and
under time-varying volatility its standard critical values no longer
control size. exuber has several fixes for this, each taking a
structurally different approach. Four of the five below
([`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md),
[`radf_sign_dm()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md),
[`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md),
[`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md))
keep the `radf_obj` class and now have full
[`summary()`](https://rdrr.io/r/base/summary.html)/[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/[`tidy()`](https://generics.r-lib.org/reference/tidy.html)/[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
support, the same as plain
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
(see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for exactly how each one plugs into the pipeline and how that was
validated);
[`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md)
doesn’t – it bundles
[`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)’s
statistic and the classic `supDF` into one union-of-rejections call with
its own class, not a `radf_obj`, so none of those four generics apply to
it, only its own
[`print()`](https://rdrr.io/r/base/print.html)/[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
[`radf_tt()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md)’s
time-deformation approach has its own dedicated vignette,
[`vignette("radf-tt")`](https://kvasilopoulos.github.io/exuber/articles/radf-tt.md);
this one covers the rest.

All of them target *non-stationary* volatility – a permanent shift or
trend in the unconditional innovation variance, not stationary
GARCH-type conditional heteroskedasticity (whose variance profile is
asymptotically flat, leaving plain
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
size-correct). So the running example is
[`sim_psy1()`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)’s
bubble driven by
[`sim_vol_break()`](https://kvasilopoulos.github.io/exuber/reference/sim_vol_break.md)
innovations, whose standard deviation triples half-way through the
sample – the case where plain
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
over-rejects most:

``` r

y <- sim_psy1(n = 200, seed = 1, e = sim_vol_break(199))
```

| Function | Paper | Approach |
|----|----|----|
| [`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md) / [`radf_sign_dm()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md) | Harvey, Leybourne & Zu (2020) | Transform to the *cumulated sign* of first differences – exactly invariant to any heteroskedasticity pattern, no bootstrap needed. `_dm` demeans first for level-shift robustness (Harvey, Leybourne, Tatlow & Zu 2025). |
| [`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md) | Harvey, Leybourne, Taylor & Zu (2024) | *Purge* volatility: divide each first difference by a kernel spot-volatility estimate, cumulate, then run ordinary PSY on the purged series – null distribution is identical to the standard homoskedastic one. |
| [`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md) / [`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md) | Harvey, Leybourne & Zu (2019) | A WLS-weighted recursive-DF statistic (`supBZ`), *weighting* rather than purging or transforming: same kernel spot-volatility estimator as [`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md), used as regression weights instead. |
| [`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md) | Harvey, Leybourne & Zu (2019) | `supDF` (classic) and `supBZ` ([`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)’s), unioned via a jointly-sized wild bootstrap – catches whichever of the two has power on a given series. |

## Sign-based: `radf_sign()`

As of 2026-08-18, this also gets full pipeline support –
[`radf_sign_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md)
computes the time-varying `badf_cv`/`bsadf_cv` boundary now, not just
the scalar critical values
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

Because
[`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md)
purges volatility and then calls
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
unmodified, it gets full pipeline support too, the simplest way of all
(no new critical-value machinery at all –
[`radf_mc_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
applies unmodified):

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

As of 2026-08-22,
[`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)
is split from the union test into its own statistic function, with
[`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md)
computing the time-varying `badf_cv`/`bsadf_cv` boundary the same way
[`radf_tt_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt_cv.md)/[`radf_sign_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md)
do (see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the validation), so it gets full pipeline support:

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

The kernel-volatility weighting that makes `supBZ`
heteroskedasticity-robust also trades away some power relative to the
other tests:
[`sim_psy1()`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)’s
default, mild bubble (30 periods at `rho = 1 + 200^-0.6`, then a
collapse) doesn’t clear `supBZ`’s 95% critical value here, even though
every other test above rejects on the same series. A stronger,
uncollapsed episode – `rho = 1.03` from `t = 120` to the sample end, on
the same volatility break – does:

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

That’s the same trade-off
[`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md)
below exists to hedge against by unioning `supBZ` with the classic
`supDF`.

## Union-of-rejections: `radf_sbz_union()`

``` r

radf_sbz_union(y, nboot = 200, seed = 1)
#> 
#> ── radf_sbz_union (minw = 27, nboot = 200) ─────────────────────────────────────
#> 
#>    series  supDF  supBZ      U  p_supDF  p_supBZ    p_U
#>   series1  9.329   1.67  9.329        0    0.085  0.005
```

`supDF` is the classic PWY statistic, `supBZ` the WLS-weighted version
(the same one
[`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)
now returns on its own), and `U` their union – each with its own
bootstrap p-value, so a series can be flagged by one without the other,
as here: `supDF` rejects, `supBZ` doesn’t, and the union `U` follows
`supDF`. `U`’s value is *defined* using a bootstrap-derived
`supDF`/`supBZ` scaling ratio, and its size guarantee requires
`supDF`/`supBZ` bootstrap draws paired from the same resampled series
per replicate – both reasons
[`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md)
can’t be reconstructed from separately calling
[`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md)
and plain
[`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md),
and why it stays a single bundled call with its own class rather than a
`radf_obj`.

## Which to reach for

- Want exact invariance to *any* heteroskedasticity pattern with no
  bootstrap at all:
  [`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md)
  (or
  [`radf_sign_dm()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md)
  if a level shift, not just volatility, is a concern). Also full
  pipeline support.
- Want to stay closest to plain
  [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md),
  no new critical-value machinery at all:
  [`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md).
- Want the WLS efficiency gain with full
  [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  support:
  [`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md) +
  [`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md).
- Want to hedge between the classic and WLS-weighted statistics on the
  same series, and don’t need
  [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html):
  [`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md)
  – the one function in this group with no pipeline support, since `U`
  bundles both statistics and their (scalar-only) joint critical value
  in one call rather than returning a `radf_obj`.
- Volatility is the whole story and a bootstrap-free, time-deformation
  approach is preferred:
  [`radf_tt()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md),
  see
  [`vignette("radf-tt")`](https://kvasilopoulos.github.io/exuber/articles/radf-tt.md).
- Volatility is genuinely unknown/complex and a bootstrap is acceptable:
  plain
  [`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
  remains the general-purpose choice.
