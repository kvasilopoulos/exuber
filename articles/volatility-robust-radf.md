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

res <- radf_sign(sim_data, minw = 20)
cv <- radf_sign_cv(n = 100, minw = 20)
summary(res, cv = cv)
#> 
#> ── Summary (minw = 20, lag = 0) ──────────────── Sign-Based MC (nboot = 2000) ──
#> 
#> psy1 :
#> # A tibble: 3 × 5
#>   stat   tstat  `90`  `95`  `99`
#>   <fct>  <dbl> <dbl> <dbl> <dbl>
#> 1 adf   -0.152 0.907  1.28  2.11
#> 2 sadf   0.937 2.32   2.65  3.42
#> 3 gsadf  2.02  2.97   3.51  4.42
#> 
#> psy2 :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf    2.56 0.907  1.28  2.11
#> 2 sadf   6.42 2.32   2.65  3.42
#> 3 gsadf 14.0  2.97   3.51  4.42
#> 
#> evans :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf    4.85 0.907  1.28  2.11
#> 2 sadf   5.76 2.32   2.65  3.42
#> 3 gsadf  6.85 2.97   3.51  4.42
#> 
#> div :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf    1.13 0.907  1.28  2.11
#> 2 sadf   2.79 2.32   2.65  3.42
#> 3 gsadf  2.95 2.97   3.51  4.42
#> 
#> blan :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf    3.38 0.907  1.28  2.11
#> 2 sadf   3.38 2.32   2.65  3.42
#> 3 gsadf  3.68 2.97   3.51  4.42
datestamp(res, cv = cv)
#> 
#> ── Datestamp (min_duration = 0) ─────────────────────────────── Sign-Based MC ──
#> 
#> psy2 :
#>   Start Peak End Duration   Signal Ongoing
#> 1    21   40 100       80 positive    TRUE
#> 
#> evans :
#>   Start Peak End Duration   Signal Ongoing
#> 1    21   84 100       80 positive    TRUE
#> 
#> blan :
#>   Start Peak End Duration   Signal Ongoing
#> 1    31   43  73       42 negative   FALSE
#> 2    76  100 100       25 positive    TRUE
```

`psy2` and `evans` clear their 99% critical values comfortably; the
others don’t on this panel – a realistic mixed result, not every series
in a demo panel is meant to look explosive.

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

res_kp <- radf_kp(sim_data, minw = 20)
cv_kp <- radf_mc_cv(n = attr(res_kp, "n"), minw = 20)
summary(res_kp, cv = cv_kp)
#> 
#> ── Summary (minw = 20, lag = 0) ────────────────── Monte Carlo (nboot = 1000) ──
#> 
#> psy1 :
#> # A tibble: 3 × 5
#>   stat   tstat   `90`    `95`  `99`
#>   <fct>  <dbl>  <dbl>   <dbl> <dbl>
#> 1 adf   -0.439 -0.377 -0.0198 0.608
#> 2 sadf   0.175  0.916  1.23   1.79 
#> 3 gsadf  1.67   1.60   1.94   2.41 
#> 
#> psy2 :
#> # A tibble: 3 × 5
#>   stat   tstat   `90`    `95`  `99`
#>   <fct>  <dbl>  <dbl>   <dbl> <dbl>
#> 1 adf   -2.40  -0.377 -0.0198 0.608
#> 2 sadf   0.918  0.916  1.23   1.79 
#> 3 gsadf  2.03   1.60   1.94   2.41 
#> 
#> evans :
#> # A tibble: 3 × 5
#>   stat   tstat   `90`    `95`  `99`
#>   <fct>  <dbl>  <dbl>   <dbl> <dbl>
#> 1 adf   -1.93  -0.377 -0.0198 0.608
#> 2 sadf  -0.714  0.916  1.23   1.79 
#> 3 gsadf  0.489  1.60   1.94   2.41 
#> 
#> div :
#> # A tibble: 3 × 5
#>   stat   tstat   `90`    `95`  `99`
#>   <fct>  <dbl>  <dbl>   <dbl> <dbl>
#> 1 adf   -2.32  -0.377 -0.0198 0.608
#> 2 sadf   0.584  0.916  1.23   1.79 
#> 3 gsadf  0.900  1.60   1.94   2.41 
#> 
#> blan :
#> # A tibble: 3 × 5
#>   stat   tstat   `90`    `95`  `99`
#>   <fct>  <dbl>  <dbl>   <dbl> <dbl>
#> 1 adf   -2.47  -0.377 -0.0198 0.608
#> 2 sadf  -1.46   0.916  1.23   1.79 
#> 3 gsadf  0.452  1.60   1.94   2.41
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

res_sbz <- radf_sbz(sim_data, minw = 20)
cv_sbz <- radf_sbz_cv(sim_data, minw = 20, nboot = 200, seed = 1)
summary(res_sbz, cv = cv_sbz)
#> 
#> ── Summary (minw = 20, lag = 0) ────────── Wild Bootstrap (SBZ) (nboot = 200) ──
#> 
#> psy1 :
#> # A tibble: 3 × 5
#>   stat   tstat  `90`  `95`  `99`
#>   <fct>  <dbl> <dbl> <dbl> <dbl>
#> 1 adf   -1.22  0.544 0.801  1.22
#> 2 sadf   0.280 1.45  1.63   2.04
#> 3 gsadf  1.05  2.03  2.76   3.27
#> 
#> psy2 :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf   -1.07 0.481 0.583 0.745
#> 2 sadf   1.53 1.76  2.72  3.49 
#> 3 gsadf  1.56 2.35  3.43  4.43 
#> 
#> evans :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf   -2.95 0.599 0.846  1.26
#> 2 sadf  -1.00 4.35  6.07   8.57
#> 3 gsadf  1.70 4.54  6.22   8.57
#> 
#> div :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf   0.660  1.10  1.48  1.97
#> 2 sadf  2.26   2.25  2.59  2.98
#> 3 gsadf 2.26   2.60  2.82  3.53
#> 
#> blan :
#> # A tibble: 3 × 5
#>   stat  tstat  `90`  `95`  `99`
#>   <fct> <dbl> <dbl> <dbl> <dbl>
#> 1 adf   -4.14 0.745 0.939  1.59
#> 2 sadf   1.40 2.65  3.63   5.65
#> 3 gsadf  2.38 3.82  4.72   6.89
```

The kernel-volatility weighting that makes `supBZ`
heteroskedasticity-robust also trades away some power relative to plain
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md) on
series whose bubbles aren’t accompanied by a volatility shift – none of
this panel clears `supBZ`’s 95% critical value here, even though several
clear plain
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)’s.
That’s the same trade-off
[`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md)
below exists to hedge against by unioning `supBZ` with the classic
`supDF`.

## Union-of-rejections: `radf_sbz_union()`

``` r

radf_sbz_union(sim_data, nboot = 200, seed = 1)
#> 
#> ── radf_sbz_union (minw = 19, nboot = 200) ─────────────────────────────────────
#> 
#>   series  supDF   supBZ      U  p_supDF  p_supBZ    p_U
#>     psy1  1.946  0.2802  1.946    0.055     0.64  0.100
#>     psy2  7.880  1.5349  7.880    0.000     0.14  0.000
#>    evans  5.283  1.9138  5.283    0.130     0.30  0.165
#>      div  1.113  2.2607  1.113    0.065     0.10  0.130
#>     blan  3.930  1.4008  3.930    0.050     0.25  0.075
```

`supDF` is the classic PWY statistic, `supBZ` the WLS-weighted version
(the same one
[`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md)
now returns on its own), and `U` their union – each with its own
bootstrap p-value, so a series can be flagged by one without the other:
`psy2` is clearly significant across all three here, while the rest of
this panel sit closer to the boundary, with `supBZ`’s p-values
consistently the least significant of the three on this draw. `U`’s
value is *defined* using a bootstrap-derived `supDF`/`supBZ` scaling
ratio, and its size guarantee requires `supDF`/`supBZ` bootstrap draws
paired from the same resampled series per replicate – both reasons
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
