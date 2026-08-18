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
structurally different approach, and – unlike most of the other new
functions – all four here keep the `radf_obj` class, so
[`summary()`](https://rdrr.io/r/base/summary.html)/[`tidy()`](https://generics.r-lib.org/reference/tidy.html)
still work the same way they do for plain
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
(see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for exactly how far each one plugs into the pipeline).
[`radf_tt()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md)’s
time-deformation approach has its own dedicated vignette,
[`vignette("radf-tt")`](https://kvasilopoulos.github.io/exuber/articles/radf-tt.md);
this one covers the rest.

| Function | Paper | Approach |
|----|----|----|
| [`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md) / [`radf_sign_dm()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md) | Harvey, Leybourne & Zu (2020) | Transform to the *cumulated sign* of first differences – exactly invariant to any heteroskedasticity pattern, no bootstrap needed. `_dm` demeans first for level-shift robustness (Harvey, Leybourne, Tatlow & Zu 2025). |
| [`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md) | Harvey, Leybourne, Taylor & Zu (2024) | *Purge* volatility: divide each first difference by a kernel spot-volatility estimate, cumulate, then run ordinary PSY on the purged series – null distribution is identical to the standard homoskedastic one. |
| [`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md) | Harvey, Leybourne & Zu (2019) | A WLS-weighted test using the same kernel volatility estimator, unioned with the classic supDF via a jointly-sized wild bootstrap. |

## Sign-based: `radf_sign()`

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
```

`psy2` and `evans` clear their 99% critical values comfortably; the
others don’t on this panel – a realistic mixed result, not every series
in a demo panel is meant to look explosive.

## Kernel-purged: `radf_kp()`

Because
[`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md)
purges volatility and then calls
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
unmodified, it gets **full** pipeline support
([`summary()`](https://rdrr.io/r/base/summary.html),
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
all work, unlike the sign-based and SBZ variants):

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

## WLS + kernel volatility with union-of-rejections: `radf_sbz_cv()`

``` r

radf_sbz_cv(sim_data, nboot = 200)
#> 
#> ── radf_sbz (minw = 19, nboot = 200) ───────────────────────────────────────────
#> 
#>   series  supDF   supBZ      U  p_supDF  p_supBZ    p_U
#>     psy1  1.946  0.2802  1.946    0.065    0.665  0.115
#>     psy2  7.880  1.5349  7.880    0.000    0.160  0.000
#>    evans  5.283  1.9138  5.283    0.105    0.270  0.130
#>      div  1.113  2.2607  1.113    0.030    0.045  0.050
#>     blan  3.930  1.4008  3.930    0.080    0.250  0.130
```

`supDF` is the classic PWY statistic, `supBZ` the WLS-weighted version,
and `U` their union – each with its own bootstrap p-value, so a series
can be flagged by one without the other (`div` here is significant on
`supDF` and the union but not on `supBZ`).

## Which to reach for

- Want exact invariance to *any* heteroskedasticity pattern with no
  bootstrap at all:
  [`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md)
  (or
  [`radf_sign_dm()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md)
  if a level shift, not just volatility, is a concern).
- Want to stay inside the full
  [`summary()`](https://rdrr.io/r/base/summary.html)/[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/[`tidy()`](https://generics.r-lib.org/reference/tidy.html)/
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  pipeline:
  [`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md)
  – the only one of this group with no gaps there.
- Want the WLS efficiency gain and a union test against the classic
  statistic:
  [`radf_sbz_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_cv.md).
- Volatility is the whole story and a bootstrap-free, time-deformation
  approach is preferred:
  [`radf_tt()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md),
  see
  [`vignette("radf-tt")`](https://kvasilopoulos.github.io/exuber/articles/radf-tt.md).
- Volatility is genuinely unknown/complex and a bootstrap is acceptable:
  plain
  [`radf_wb_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
  remains the general-purpose choice.
