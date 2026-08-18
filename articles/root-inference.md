# Root Inference: How Fast Is the Bubble Growing

``` r

library(exuber)
```

## A different question from “is there a bubble”

[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)/[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
(and the `dating_*()`/`_test()` families) all answer some version of “is
there a bubble, and when did it happen.” None of them say anything about
its *magnitude*: once an explosive episode is dated, how fast is the
underlying autoregressive root actually growing?
[`explosive_root()`](https://kvasilopoulos.github.io/exuber/reference/explosive_root.md),
[`root_ci()`](https://kvasilopoulos.github.io/exuber/reference/root_ci.md)
and
[`root_ci_datestamp()`](https://kvasilopoulos.github.io/exuber/reference/root_ci_datestamp.md)
(Phillips & Magdalinos 2007; Guo, Sun & Wang 2019) answer exactly that,
as a follow-up step *after* detection and dating, not a replacement for
either.

They fit a no-intercept AR(1), `y_t = rho * y_{t-1} + e_t`, over a given
sub-sample and report `rho`’s estimate together with a confidence
interval and an implied **doubling time** (`log(2) / log(rho)`: how many
periods until the bubble doubles in size at the estimated growth rate).
None of the three carry the `radf_obj` class – see
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

Then estimate the root over the detected episode:

``` r

est <- explosive_root(y, ds[["series1"]]$Start[1], ds[["series1"]]$End[1])
est
#> $rho
#> [1] 1.041945
#> 
#> $se
#> [1] 0.006318027
#> 
#> $t_stat
#> [1] 6.638914
#> 
#> $n
#> [1] 17
```

`est$rho` recovers something close to the true 1.04.
[`root_ci()`](https://kvasilopoulos.github.io/exuber/reference/root_ci.md)
turns the point estimate into an interval and a doubling time:

``` r

root_ci(est) # normal-t interval (Guo, Sun & Wang 2019)
#> $rho
#> [1] 1.041945
#> 
#> $rho_ci
#> [1] 1.029562 1.054328
#> 
#> $doubling_time
#> [1] 16.86941
#> 
#> $doubling_time_ci
#> [1] 13.10209 23.79234
root_ci(est, type = "cauchy") # fixed-root Cauchy interval (Phillips & Magdalinos 2007)
#> $rho
#> [1] 1.041945
#> 
#> $rho_ci
#> [1] 0.5007196 1.5831701
#> 
#> $doubling_time
#> [1] 16.86941
#> 
#> $doubling_time_ci
#> [1]  1.508714 -1.002079
```

The two interval types answer slightly different questions.
`type = "normal"` (the default) is the safer choice under drift/weak
dependence and gives a noticeably tighter interval here;
`type = "cauchy"` assumes a genuinely *fixed*, non-drifting root and,
because a Cauchy distribution has much fatter tails than a normal,
produces a visibly wider interval even at the same nominal level.

## Every episode at once

For a
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
result with more than one episode,
[`root_ci_datestamp()`](https://kvasilopoulos.github.io/exuber/reference/root_ci_datestamp.md)
runs
[`explosive_root()`](https://kvasilopoulos.github.io/exuber/reference/explosive_root.md)/[`root_ci()`](https://kvasilopoulos.github.io/exuber/reference/root_ci.md)
on each one without a manual loop:

``` r

root_ci_datestamp(r, ds)
#> $series1
#>   Start End      rho rho_lower rho_upper doubling_time doubling_time_lower
#> 1    83 100 1.041945  1.029562  1.054328      16.86941            13.10209
#>   doubling_time_upper
#> 1            23.79234
```

Root inference on a very short episode is close to meaningless (too few
points to estimate an AR(1) coefficient precisely) – filter with
`datestamp(..., min_duration = ...)` before piping in, rather than
expecting
[`root_ci_datestamp()`](https://kvasilopoulos.github.io/exuber/reference/root_ci_datestamp.md)
to second-guess what counts as “too short.”
