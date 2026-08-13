# Reverse-Regression Dating of Crisis Origination and Market Recovery

`radf_recovery` implements Phillips & Shi (2014)'s reverse- regression
dating: reverses the series, runs
[`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s
existing bsadf recursion on it, and locates the first up-crossing of a
reversal-calibrated critical value boundary (the market recovery date)
followed by the next down-crossing (the crisis/collapse origination date
in the original series), then maps both back to the original time index.

## Usage

``` r
radf_recovery(
  data,
  minw = NULL,
  lag = 0,
  nrep = 1000L,
  sig_lvl = 95,
  seed = NULL
)
```

## Arguments

- data:

  A univariate or multivariate numeric time series object, a numeric
  vector or matrix, or a data.frame. A column may have leading and/or
  trailing `NA` values (an uneven/unbalanced panel where series enter or
  exit the sample at different times) – those periods are filled with
  `NA` in `badf`/`bsadf` and excluded from that series' `adf`/`sadf`/
  `gsadf`. Interior `NA` values (a gap in the middle of a series) are
  not supported. When any series is padded this way, the panel statistic
  (`bsadf_panel`/`gsadf_panel`) is not available and is returned as
  `NA`, with a warning.

- minw:

  A positive integer. The minimum window size (default = \\(0.01 +
  1.8/\sqrt(T))T\\, where T denotes the sample size).

- lag:

  A non-negative integer. The lag length of the Augmented Dickey-Fuller
  regression (default = 0L).

- nrep:

  Number of Monte Carlo replications for
  [`radf_recovery_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_recovery_cv.md)'s
  critical value.

- sig_lvl:

  Significance level, one of `90`, `95`, `99`.

- seed:

  Optional seed for the Monte Carlo draws.

## Value

An object of class `radf_recovery_obj`: a list with `f_c`/`f_r` (the
estimated dates, `NA` if not identified), `detected` (logical, whether
an up-crossing was found at all), and `censored` (logical, whether `f_c`
is left-censored by the start of the reverse-time sample).

## Details

Two dates are returned per series: `f_c`, the crisis origination
(collapse-onset) date – a reverse-regression-derived alternative to the
collapse date
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
already dates from the forward test – and `f_r`, the market recovery
date, always `f_c <= f_r` by construction (the down-crossing is searched
only after the up-crossing). If no up-crossing is found, neither date is
identified (`NA`, `detected = FALSE`). If an up-crossing is found but no
subsequent down-crossing occurs before the reverse-time sample is
exhausted, `f_c` is `NA` and `censored = TRUE` (the crisis origination
predates the observed sample).

## Caveats

**\[experimental\]**

**Validation status (2026-08-10), reported honestly rather than
silently**: `f_r` (recovery date) validates well against synthetic
collapse-then-recovery data – bias in the same range the paper's own
Monte Carlo reports (a few observations early). `f_c` (crisis
origination date) shows a materially larger residual bias in Monte Carlo
checks, and the empirical false-detection rate under a pure random-walk
null (n=100, minw=20, 95\\ than comparable forward-test numbers
elsewhere in this package. One real synthetic-DGP artifact (a level-jump
at a regime boundary producing a spurious spike) was found and fixed
during validation, but the residual `f_c` bias/false-detection elevation
was not fully resolved – plausibly genuine finite-sample noise in the
paper's own literal first-down-crossing rule (eq. 9's `inf` has no
persistence requirement, so a transient dip below the boundary is enough
to trigger a premature `f_c`), but this has not been ruled out against a
subtler implementation issue. Treat `f_c` and the overall detection rate
as exploratory pending further validation; see
docs/enhancements/dating-and-root-inference.md for the full numbers. The
same short pointer is emitted as a message when this function is called
(see [`suppressMessages`](https://rdrr.io/r/base/message.html) to
silence it) and stored as `attr(x, "caveat")` on the returned object.

## References

Phillips, P. C. B., & Shi, S. (2014). Financial Bubble Implosion and
Reverse Regression. Cowles Foundation Discussion Paper No. 1967, Yale
University. Published in Econometric Theory.

## See also

[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)
for the (forward, non-reversed) origination/collapse dating this
complements.

## Examples

``` r
# \donttest{
res <- radf_recovery(sim_data, nrep = 200)
#> Experimental. f_c and the overall false-detection rate are exploratory pending further validation; see ?radf_recovery, Caveats section.
print(res)
#> 
#> ── radf_recovery (n = 100, minw = 19, level = 95%) ─────────────────────────────
#> 
#> ℹ Experimental. f_c and the overall false-detection rate are exploratory pending further validation; see ?radf_recovery, Caveats section.
#> 
#>   series   f_c   f_r  detected  censored
#>     psy1  <NA>  <NA>     FALSE     FALSE
#>     psy2  <NA>  <NA>     FALSE     FALSE
#>    evans  <NA>  <NA>     FALSE     FALSE
#>      div  <NA>  <NA>     FALSE     FALSE
#>     blan  <NA>  <NA>     FALSE     FALSE
#> 
# }
```
