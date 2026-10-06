# Experimental Methods: radf_recovery() and datestamp(option = 'svadf')

``` r

library(exuber)
```

## What “experimental” means here

Most methods in exuber implement the procedure of a peer-reviewed paper
and pass the package’s standard validation. That validation consists of
a formula-exact check against a brute-force reimplementation, a lookup
against published tables, a Monte Carlo check of size, and a check of
power against a true alternative.
[`radf_recovery()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md)
and `datestamp(option = "svadf")` went through the same validation and
both give useful results, but each has one disclosed gap that keeps it
below the standard. For that reason they print an “Experimental” badge
and emit a caveat message when called. Treat their output as a guide to
where episodes lie, and do not assume it is as well calibrated as the
rest of the package.

## `radf_recovery()`: dating a collapse and a recovery

This function uses the reverse-regression idea of Phillips & Shi (2014).
We reverse the series in time, run the BSADF recursion that
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
already computes, and map the crossing dates back to the original time
axis. In the reversed series a collapse followed by a recovery turns the
collapse into an explosive regime and the recovery into the end of that
regime, so the forward machinery run backwards dates both.

``` r

# sim_ps1(): unit root -> explosive (40-60) -> collapse (61-70) -> recovery (71+)
y <- sim_ps1(n = 100, seed = 2)
res <- radf_recovery(y, minw = 15, nrep = 200, seed = 1)
res
#> 
#> ── radf_recovery (n = 100, minw = 15, level = 95%) ─────────────────────────────
#> 
#> ℹ Experimental. f_c and the overall false-detection rate are exploratory pending further validation; see ?radf_recovery, Caveats section.
#> 
#>    series  f_c  f_r  detected  censored
#>   series1   57   67      TRUE     FALSE
```

The estimate `f_c` (crisis onset, 57) falls just before the true
collapse start (61). The estimate `f_r` (recovery, 67) falls after it,
inside the collapse regime and before the true recovery date (70). The
two dates come out in the right order by construction, because the
down-crossing search only starts at the up-crossing. The disclosed gap
is that `f_c` and the overall false-detection rate are exploratory until
they are validated further (see the Caveats section of
[`?radf_recovery`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md)).
The ordering of the dates is reliable, but the calibration of false
alarms is not yet.

## `datestamp(option = "svadf")`: a preprint

This option implements Sarkar & Wells (2026), an arXiv preprint that has
not been peer reviewed. Every other paper implemented in the package has
been, so the evidence behind this method is weaker. Its statistic is the
`badf` sequence that
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
already computes, compared against two closed-form thresholds that
depend only on the sample size and come from the applied methodology of
the paper. It is an option of
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
and not a separate function.

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

`psy1` and `psy2` receive clear origination and collapse dates, while
`evans`, `div` and `blan` never cross the threshold in this panel.

## Using them responsibly

Both methods are worth using. The date ordering from
[`radf_recovery()`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery.md)
and the point statistic from `datestamp(option = "svadf")` are reliable.
Neither should be the only basis for a claim about false-alarm rates or
exact calibration, though. When that matters, prefer
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md) and
[`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md),
or one of the peer-reviewed alternatives in
[`vignette("alternative-tests")`](https://kvasilopoulos.github.io/exuber/articles/alternative-tests.md)
and
[`vignette("dating-methods")`](https://kvasilopoulos.github.io/exuber/articles/dating-methods.md).
Treat these two methods as a second opinion until their caveats are
resolved. The caveats are listed in the replication notes for dating and
volatility-robustness.
