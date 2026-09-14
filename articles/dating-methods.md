# Dating Methods: Alternatives to datestamp()

``` r

library(exuber)
```

## Why these exist alongside `datestamp()`

[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
applies PSY’s own rule to a
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
result: a bubble runs from the first point the recursive statistic
crosses its critical value to the first point it drops back below. That
rule is simple and well understood, but it is not the only way to *date*
a bubble once you already believe one is there. The `dating_*()` family
instead fits an explicit regime model – unit-root, then explosive, then
unit-root again – directly to the raw series by minimizing residual sum
of squares (SSR), and picks the break dates that fit that model best.
They take no critical value at all: given a window believed to contain
(at most) one bubble, they answer “where exactly does it start and end,”
not “is there one.”

| Function | Paper | Idea |
|----|----|----|
| [`dating_hls()`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md) | Harvey, Leybourne & Sollis (2017) | Fits 4 candidate regime-dummy models (with/without a distinct collapse regime, with/without recovery) via closed-form segment SSR; BIC picks among them. |
| [`dating_knp()`](https://kvasilopoulos.github.io/exuber/reference/dating_knp.md) | Kejriwal, Nguyen & Perron (2025) | Same model as HLS’s Model 2, but proves the plain SSR minimizer is *inconsistent* – it converges to the collapse date, not the origination date – and fixes it by omitting one squared residual from the objective. |
| [`dating_pdc()`](https://kvasilopoulos.github.io/exuber/reference/dating_pdc.md) | Pang, Du & Chong (2021); Kurozumi & Skrobotov (2023) | Assumes a fixed 3- or 4-regime structure and finds each breakpoint *sequentially* in closed form (collapse first, since it is stochastically dominant), with no BIC step. |
| [`dating_hlw()`](https://kvasilopoulos.github.io/exuber/reference/dating_hlw.md) | Harvey, Leybourne & Whitehouse (2020) | A wrapper: runs [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)/[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md) first to find *how many* episodes and roughly where, then applies HLS-style fitting independently within each detected window. |

All four take no `radf_cv`, so they don’t plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/[`tidy()`](https://generics.r-lib.org/reference/tidy.html)/
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html) –
see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the full pipeline picture. Each prints its own dating table instead.

## A single bubble, four verdicts

One simulated series from
[`sim_ps1()`](https://kvasilopoulos.github.io/exuber/reference/sim_ps1.md),
Phillips & Shi (2018)’s own single-bubble DGP: a unit-root run-up, a
genuine explosive regime (true origination at 40), a mildly-integrated
collapse regime (starting at 61) and a return to a unit root (recovery
at 70) – exactly the regime structure these estimators are built for.

``` r

y <- sim_ps1(n = 100, seed = 1)
```

True origination is 40, true collapse is 61. Running all four:

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
#> ── dating_knp (n = 100, trim = 0.05, omit = TRUE) ──────────────────────────────
#> 
#>    series  origination  collapse   delta
#>   series1           60        70  0.9178
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
both select the 4-regime model and land within a point of every true
date (39/60/70 against 40/61/70);
[`dating_pdc()`](https://kvasilopoulos.github.io/exuber/reference/dating_pdc.md)
is a point or two early on both (38/59).
[`dating_knp()`](https://kvasilopoulos.github.io/exuber/reference/dating_knp.md)
is the visible failure mode here: its model assumes an *instantaneous*
collapse (unit root resuming from a shifted level), so
[`sim_ps1()`](https://kvasilopoulos.github.io/exuber/reference/sim_ps1.md)’s
ten-period mildly-integrated crash (61-70) is a regime it has no room
for, and it dates that crash as the episode instead, reporting 60/70 – a
real, not contrived, consequence of a model mismatch, not of the
estimator’s bias correction.

The point of running all four side by side isn’t that one is “correct” –
it’s that these are genuinely different estimators with different
failure modes, and disagreement between them on real data is
informative, not a bug to be resolved by picking a favorite.

## Which to reach for

- Believe there is exactly **one** bubble in the window and want the
  best-fitting regime model (with or without a distinct
  collapse/recovery regime):
  [`dating_hls()`](https://kvasilopoulos.github.io/exuber/reference/dating_hls.md).
- Same setting, but origination-date accuracy matters more than
  collapse-date accuracy (KNP’s own finding: the naive estimator is
  biased toward the collapse date):
  [`dating_knp()`](https://kvasilopoulos.github.io/exuber/reference/dating_knp.md).
- Want closed-form, no BIC model search, and are willing to fix the
  number of regimes up front:
  [`dating_pdc()`](https://kvasilopoulos.github.io/exuber/reference/dating_pdc.md)
  (add `weights` for the volatility-corrected variant).
- Don’t know how many episodes there are, or want dating anchored to an
  actual
  [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)/[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
  detection first:
  [`dating_hlw()`](https://kvasilopoulos.github.io/exuber/reference/dating_hlw.md).
