# Root Inference: How Fast Is the Bubble Growing

``` r

library(exuber)
```

## A different question from “is there a bubble”

[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)/[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
(and the `dating_*()`/`_test()`/`monitor_*()` families) all answer some
version of “is there a bubble, and when did it happen.” None of them say
anything about its *magnitude*: once an explosive episode is dated, how
fast is the underlying autoregressive root actually growing?
[`rootstamp()`](https://kvasilopoulos.github.io/exuber/reference/rootstamp.md)
(Phillips & Magdalinos 2007; Guo, Sun & Wang 2019) answers exactly that,
as a follow-up step *after* detection and dating, not a replacement for
either.

It fits a no-intercept AR(1), `y_t = rho * y_{t-1} + e_t`, over a given
sub-sample and reports `rho`’s estimate together with a confidence
interval and an implied **doubling time** (`log(2) / log(rho)`: how many
periods until the bubble doubles in size at the estimated growth rate).
Two methods, for two different starting points – a plain numeric
sub-sample (fit once, one CI), or a `radf_obj` plus its
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
result (fit every episode at once, no manual loop). Neither method’s
return value carries the `radf_obj` class itself, so
[`rootstamp()`](https://kvasilopoulos.github.io/exuber/reference/rootstamp.md)
doesn’t plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/[`tidy()`](https://generics.r-lib.org/reference/tidy.html)/[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
– see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md).

## Detect, date, then estimate the root

A unit-root run-up followed by a genuine explosive regime with
`rho = 1.04`:

``` r

set.seed(2026)
burn <- cumsum(rnorm(60))
bubble <- burn[length(burn)] * 1.04^(1:40) + cumsum(rnorm(40, sd = 0.5))
y <- c(burn, bubble)
```

Detect and date it first, the ordinary way:

``` r

r <- radf(y, minw = 20)
cv <- radf_mc_cv(length(y), minw = 20, nrep = 300, seed = 4)
ds <- datestamp(r, cv = cv, min_duration = 3)
ds
#> 
#> ── Datestamp (min_duration = 3) ───────────────────────────────── Monte Carlo ──
#> 
#> series1 :
#>   Start Peak End Duration   Signal Ongoing
#> 1    83  100 100       18 negative    TRUE
```

Then estimate the root over the detected episode – the default method
takes the sub-sample directly, sliced by the episode’s own
`Start`/`End`:

``` r

ep <- ds[["series1"]]
rootstamp(y[ep$Start[1]:ep$End[1]]) # normal-t interval (Guo, Sun & Wang 2019), true rho = 1.04
#> 
#> ── rootstamp (n = 17, level = 95%, type = normal) ──────────────────────────────
#> 
#>     rho        se  t_stat  rho_lower  rho_upper  doubling_time  dt_lower
#>   1.042  0.006318   6.639       1.03      1.054          16.87      13.1
#>   dt_upper
#>      23.79
rootstamp(y[ep$Start[1]:ep$End[1]], type = "cauchy") # fixed-root Cauchy interval (Phillips & Magdalinos 2007)
#> 
#> ── rootstamp (n = 17, level = 95%, type = cauchy) ──────────────────────────────
#> 
#>     rho        se  t_stat  rho_lower  rho_upper  doubling_time  dt_lower
#>   1.042  0.006318   6.639     0.5007      1.583          16.87     1.509
#>   dt_upper
#>     -1.002
```

`rho` recovers something close to the true 1.04, alongside `rho_ci` and
the implied `doubling_time`/`doubling_time_ci`. The two interval types
answer slightly different questions. `type = "normal"` (the default) is
the safer choice under drift/weak dependence and gives a noticeably
tighter interval here; `type = "cauchy"` assumes a genuinely *fixed*,
non-drifting root and, because a Cauchy distribution has much fatter
tails than a normal, produces a visibly wider interval even at the same
nominal level.

## Every episode at once

For a
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
result with more than one episode, the `radf_obj` method runs the
default method on each one without a manual loop – pass the original
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
result and the
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
result together:

``` r

rootstamp(r, ds)
#> 
#> ── rootstamp (level = 95%, type = normal) ──────────────────────────────────────
#> 
#> series1 :
#>   Start End   rho rho_lower rho_upper doubling_time doubling_time_lower
#> 1    83 100 1.042      1.03     1.054         16.87                13.1
#>   doubling_time_upper
#> 1               23.79
```

Root inference on a very short episode is close to meaningless (too few
points to estimate an AR(1) coefficient precisely) – filter with
`datestamp(..., min_duration = ...)` before piping in, rather than
expecting this method to second-guess what counts as “too short.”
