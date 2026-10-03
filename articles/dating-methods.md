# Dating Methods: Alternatives to datestamp()

``` r

library(exuber)
```

## Why these exist alongside `datestamp()`

[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
applies the rule of Phillips, Shi and Yu to a
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
result: a bubble runs from the first point at which the recursive
statistic crosses its critical value to the first point at which it
falls back below it. The rule is simple and well understood, but it is
not the only way to date a bubble once you already believe that one
exists. The `dating_*()` family fits an explicit regime model to the raw
series: a unit root, then an explosive regime, then a unit root again.
It chooses the break dates that minimize the residual sum of squares
(SSR). These functions need no critical value. Given a window that
contains at most one bubble, they tell you where the bubble starts and
ends, and they do not tell you whether there is one.

| Function | Paper | Idea |
|----|----|----|
| [`dating_hls()`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md) | Harvey, Leybourne & Sollis (2017) | Fits four candidate regime-dummy models (with or without a distinct collapse regime, with or without recovery) using closed-form segment SSR, and the BIC picks among them. |
| [`dating_knp()`](https://kvasilopoulos.github.io/exuber/reference/dating_knp.md) | Kejriwal, Nguyen & Perron (2025) | Uses the same model as Model 2 of HLS. The authors prove that the plain SSR minimizer is inconsistent, because it converges to the collapse date and not to the origination date, and they correct this by omitting one squared residual from the objective. |
| [`dating_pdc()`](https://kvasilopoulos.github.io/exuber/reference/dating_pdc.md) | Pang, Du & Chong (2021); Kurozumi & Skrobotov (2023) | Assumes a fixed structure of three or four regimes and finds each breakpoint sequentially in closed form, starting with the collapse because it is stochastically dominant. There is no BIC step. |
| [`dating_hlw()`](https://kvasilopoulos.github.io/exuber/reference/dating_hlw.md) | Harvey, Leybourne & Whitehouse (2020) | A wrapper that first runs [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md) and [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md) to find how many episodes there are and roughly where they lie, and then applies HLS-style fitting separately within each detected window. |

None of the four takes a `radf_cv` object, so they do not work with
[`summary()`](https://rdrr.io/r/base/summary.html),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) or
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
Each prints its own dating table instead.
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
describes the full pipeline.

## A single bubble, four estimates

We use one series simulated with
[`sim_ps1()`](https://kvasilopoulos.github.io/exuber/reference/sim_ps1.md),
the single-bubble data generating process of Phillips & Shi (2018). It
has a unit-root run-up, an explosive regime that starts at 40, a mildly
integrated collapse regime that starts at 61, and a return to a unit
root at 70. This is the regime structure that these estimators are built
for.

``` r

y <- sim_ps1(n = 100, seed = 1)
```

The true origination date is 40 and the true collapse date is 61. We run
all four estimators:

``` r

dating_hls(y, trim = 0.05)
#> 
#> ── dating_hls (n = 100, trim = 0.05) ───────────────────────────────────────────
#> 
#>    series  model  origination  collapse  recovery
#>   series1      4           39        60        70
```

``` r

dating_knp(y, trim = 0.05)
#> 
#> ── dating_knp (n = 100, trim = 0.05, omit = TRUE, breaks = 2 ───────────────────
#> 
#>    series  bubble  origination  collapse   delta
#>   series1       1           60        70  0.9178
```

``` r

dating_pdc(y, regimes = 3, trim = 0.05)
#> 
#> ── dating_pdc (n = 100, regimes = 3, type = ols) ───────────────────────────────
#> 
#>    series  origination  collapse
#>   series1           38        59
```

``` r

dating_hlw(y, trim = 0.1, nboot = 199, seed = 1)
#> 
#> ── dating_hlw (n = 100, trim = 0.1) ────────────────────────────────────────────
#> 
#> series1:
#>  model origination collapse recovery
#>      4          39       60       70
```

On this draw,
[`dating_hls()`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md)
and
[`dating_hlw()`](https://kvasilopoulos.github.io/exuber/reference/dating_hlw.md)
both select the four-regime model and land within one period of every
true date (39, 60 and 70 against 40, 61 and 70).
[`dating_pdc()`](https://kvasilopoulos.github.io/exuber/reference/dating_pdc.md)
is one or two periods early on both dates (38 and 59).
[`dating_knp()`](https://kvasilopoulos.github.io/exuber/reference/dating_knp.md)
performs worst here. Its model assumes an instantaneous collapse, with
the unit root resuming from a shifted level, so it has no room for the
ten-period mildly integrated crash (61 to 70) that
[`sim_ps1()`](https://kvasilopoulos.github.io/exuber/reference/sim_ps1.md)
generates. It dates that crash as the episode instead and reports 60 and
70. This follows from the mismatch between the model and the data and
not from the estimator’s bias correction.

We run all four side by side to show that they are different estimators
with different failure modes, and not to single out one as correct. When
they disagree on real data, the disagreement is informative, and it
should not be settled by picking a favorite.

## Which to reach for

- If you believe the window contains exactly one bubble and you want the
  best-fitting regime model, with or without a distinct collapse and
  recovery regime, use
  [`dating_hls()`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md).
- If you have the same setting but the accuracy of the origination date
  matters more than that of the collapse date, use
  [`dating_knp()`](https://kvasilopoulos.github.io/exuber/reference/dating_knp.md).
  Its authors find that the naive estimator is biased toward the
  collapse date.
- If you want closed-form estimates with no BIC model search and can fix
  the number of regimes in advance, use
  [`dating_pdc()`](https://kvasilopoulos.github.io/exuber/reference/dating_pdc.md).
  Add `weights` for the volatility-corrected variant.
- If you do not know how many episodes there are, or you want the dating
  to start from an actual
  [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
  and
  [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
  detection, use
  [`dating_hlw()`](https://kvasilopoulos.github.io/exuber/reference/dating_hlw.md).
