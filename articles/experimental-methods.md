# Experimental Methods: radf_recovery() and datestamp(option = 'svadf')

``` r

library(exuber)
```

## Why “experimental” is a real, load-bearing label here

Most of exuber’s methods implement a peer-reviewed paper’s own procedure
and pass the package’s standard validation pass (formula-exact check
against a brute-force reimplementation, published-table lookup, Monte
Carlo size, power against a genuine alternative).
[`radf_recovery()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md)
and `datestamp(option = "svadf")` both run that same pass and both come
back genuinely useful – but each has one specific, disclosed gap that
keeps it below that bar. They print and emit a caveat message at call
time for exactly that reason; treat their output as directional, not as
calibrated as the rest of the package.

## `radf_recovery()`: dating a collapse *and* a recovery

Phillips & Shi (2014)’s reverse-regression idea: reverse the series in
time, run the same BSADF recursion
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
already computes, and map the crossing dates back. A
collapse-then-recovery episode, reversed, turns the collapse into an
explosive regime and the recovery into its unwind – so the existing
forward machinery, run backwards, dates both.

``` r

set.seed(2)
n1 <- 40; n2 <- 25; n3 <- 35
expansion <- 100 * 1.03^(1:n1) + cumsum(rnorm(n1, sd = 1))
collapse <- expansion[n1] * 0.5^((1:n2) / n2) + cumsum(rnorm(n2, sd = 1))
recovery <- collapse[n2] + cumsum(rnorm(n3, sd = 1)) + (1:n3) * 0.5
y <- c(expansion, collapse, recovery) # expansion -> collapse -> recovery
res <- radf_recovery(y, minw = 15, nrep = 200, seed = 1)
res
#> 
#> ── radf_recovery (n = 100, minw = 15, level = 95%) ─────────────────────────────
#> 
#> ℹ Experimental. f_c and the overall false-detection rate are exploratory pending further validation; see ?radf_recovery, Caveats section.
#> 
#>    series  f_c  f_r  detected  censored
#>   series1   35   62      TRUE     FALSE
```

`f_c` (crisis onset) lands near the true collapse start (40) and `f_r`
(recovery) after it (62), in the right order – by construction, since
the down-crossing search only ever starts at the up-crossing. The
disclosed gap: `f_c` and the overall false-detection rate are
exploratory pending further validation (see
[`?radf_recovery`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md),
Caveats section) – the date-ordering property is solid, the false-alarm
calibration is not yet.

## `datestamp(option = "svadf")`: a non-peer-reviewed preprint

Sarkar & Wells (2026, arXiv, not yet peer reviewed) – flagged explicitly
because that is a different evidentiary bar than every other paper this
package implements. Its statistic turns out to be exactly
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)’s
own `badf` sequence compared against two closed-form, sample-size-only
thresholds from the paper’s own applied methodology (no new estimation
machinery needed), which is why it was cheap to add – as a
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
option rather than a separate entry point, despite the preprint caveat.

``` r

res <- radf(sim_data, lag = 0)
datestamp(res, option = "svadf", min_duration = psy_ds(nrow(sim_data)))
#> 
#> ── Datestamp (min_duration = 5) ──────────────── SV-ADF (Sarkar & Wells 2026) ──
#> 
#> ℹ Experimental. Sarkar & Wells (2026) is a non-peer-reviewed preprint; see ?datestamp, Caveats section.
#> 
#> psy1 :
#>   Start Peak End Duration   Signal Ongoing
#> 1    48   48  49        1 positive   FALSE
#> 
#> psy2 :
#>   Start Peak End Duration   Signal Ongoing
#> 1    23   23  24        1 positive   FALSE
```

`psy1` and `psy2` get clean origination/collapse dates; `evans`, `div`
and `blan` don’t cross the threshold on this panel at all – again a
realistic, not cherry-picked, mixed result.

## Using them responsibly

Both are worth using –
[`radf_recovery()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md)’s
date *ordering* result and `datestamp(option = "svadf")`’s point
statistic are both solid – but neither should be the sole basis for a
claim about false-alarm rates or exact calibration. Prefer
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)/[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
or one of the peer-reviewed alternatives in
[`vignette("alternative-tests")`](https://kvasilopoulos.github.io/exuber/articles/alternative-tests.md)/[`vignette("dating-methods")`](https://kvasilopoulos.github.io/exuber/articles/dating-methods.md)
when that matters, and treat these two as a second opinion rather than
the primary one until their own caveats are resolved (tracked in
`docs/enhancements/dating-and-root-inference.md` and
`volatility-robustness.md`).
