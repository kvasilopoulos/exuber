# Root Inference: How Fast Is the Bubble Growing

``` r

library(exuber)
```

## A different question from “is there a bubble”

[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md),
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
and the `dating_*()`, `_test()`,
[`monitor()`](https://kvasilopoulos.github.io/exuber/reference/monitor.md)
and `monitor_*()` families all answer some version of the question of
whether there is a bubble and when it happened. None of them says
anything about its magnitude. Once an explosive episode is dated, how
fast is the underlying autoregressive root growing?
[`rootstamp()`](https://kvasilopoulos.github.io/exuber/reference/rootstamp.md)
(Phillips & Magdalinos 2007; Guo, Sun & Wang 2019) answers that
question. It is a follow-up step after detection and dating, and it
replaces neither.

The function fits a no-intercept AR(1), `y_t = rho * y_{t-1} + e_t`,
over a given sub-sample. It reports the estimate of `rho` with a
confidence interval and the implied doubling time, `log(2) / log(rho)`,
which is the number of periods the bubble needs to double in size at the
estimated growth rate. There are two methods for two starting points.
The default method takes a numeric sub-sample and fits it once, with one
confidence interval. The `radf_obj` method takes a `radf_obj` together
with its
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
result and fits every episode at once, with no manual loop. Neither
method returns the `radf_obj` class, so
[`rootstamp()`](https://kvasilopoulos.github.io/exuber/reference/rootstamp.md)
does not work with [`summary()`](https://rdrr.io/r/base/summary.html),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) or
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
(see
[`vignette("naming-and-analysis")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)).

## Detect, date, then estimate the root

The series has a unit-root run-up followed by an explosive regime with
`rho = 1.04`:

``` r

y <- sim_psy1(n = 100, te = 60, tf = 100, c = 0.04, alpha = 0, sigma = 1, seed = 2026)
```

We first detect and date the episode in the usual way:

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
#> 1    63  100 100       38 positive    TRUE
```

Then we estimate the root over the detected episode. The default method
takes the sub-sample directly, which we slice with the `Start` and `End`
of the episode:

``` r

ep <- ds[["series1"]]
rootstamp(y[ep$Start[1]:ep$End[1]]) # normal-t interval (Guo, Sun & Wang 2019), true rho = 1.04
#> 
#> ── rootstamp (n = 37, sig_lvl = 95%, type = normal) ────────────────────────────
#> 
#>    rho         se  t_stat  rho_lower  rho_upper  doubling_time  dt_lower
#>   1.04  0.0007295    54.2      1.038      1.041          17.87     17.26
#>   dt_upper
#>      18.53
rootstamp(y[ep$Start[1]:ep$End[1]], type = "cauchy") # fixed-root Cauchy interval (Phillips & Magdalinos 2007)
#> 
#> ── rootstamp (n = 37, sig_lvl = 95%, type = cauchy) ────────────────────────────
#> 
#>    rho         se  t_stat  rho_lower  rho_upper  doubling_time  dt_lower
#>   1.04  0.0007295    54.2     0.7955      1.284          17.87     2.776
#>   dt_upper
#>      -3.03
```

The estimate of `rho` is close to the true value of 1.04. The output
also reports `rho_ci` and the implied `doubling_time` and
`doubling_time_ci`. The two interval types answer slightly different
questions. `type = "normal"`, the default, is the safer choice under
drift or weak dependence, and it gives a noticeably tighter interval
here. `type = "cauchy"` assumes a fixed root that does not drift. A
Cauchy distribution has much fatter tails than a normal one, so this
interval is visibly wider even at the same nominal level.

## Every episode at once

When a
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
result contains more than one episode, the `radf_obj` method runs the
default method on each of them without a manual loop. Pass the original
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
result and the
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
result together:

``` r

rootstamp(r, ds)
#> 
#> ── rootstamp (sig_lvl = 95%, type = normal) ────────────────────────────────────
#> 
#> series1 :
#>   Start End  rho rho_lower rho_upper doubling_time doubling_time_lower
#> 1    63 100 1.04     1.038     1.041         17.87               17.26
#>   doubling_time_upper
#> 1               18.53
```

Root inference on a very short episode is close to meaningless, because
there are too few points to estimate an AR(1) coefficient precisely.
Filter the episodes with `datestamp(..., min_duration = ...)` before
passing them in. The method does not decide for you what counts as too
short.
