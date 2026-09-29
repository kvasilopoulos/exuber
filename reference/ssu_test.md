# Stochastic Unit Root Bubble Test (Kurozumi & Nishi 2025)

`ssu_test` implements the SSU and GSSU statistics of Kurozumi & Nishi
(2025): sup-type tests for a bubble based on testing for a stochastic
(rather than deterministic) unit root in the *squared* first
differences, `(Delta y_t)^2 = mu2 + omega*y_{t-1}^2 + eta_t`,
bias-corrected against its dependence on the correlation between this
regression's and the plain ADF regression's innovations.

## Usage

``` r
ssu_test(
  data,
  minw = NULL,
  sig_lvl = 95,
  type = c("ssu", "gssu"),
  union = FALSE,
  cv = NULL
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

  Minimum window; defaults to
  [`psy_minw`](https://kvasilopoulos.github.io/exuber/reference/psy_minw.md)
  for `"ssu"` and the paper's `floor(n * (-0.004 + 2.24/sqrt(n)))` for
  `"gssu"` (the values Table I is computed at).

- sig_lvl:

  Significance level on the package-wide 0-100 scale, one of `90`, `95`,
  `99` (the levels Kurozumi & Nishi's Table I tabulates).

- type:

  `"ssu"` or `"gssu"`.

- union:

  Logical; also run the union-of-rejections procedure with SADF
  (`"ssu"`) or GSADF (`"gssu"`).

- cv:

  Critical values for the SADF/GSADF side of the union, as from
  [`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
  for `lag = 0`; defaults to the precomputed ones (fetched on first
  use).

## Value

An object of class `ssu_test_obj`: a list with the statistic path
(`stat`, one value per candidate end point from `minw` to `n`; for GSSU
the sup over window starts at each end point), the constant `crit` from
Table I, `sadf` (the maximum, compared against `crit`) and `detected`.
With `union = TRUE` also `adf_stat` (SADF or GSADF), `union_stat`,
`union_crit` and `union_detected`.

## Details

A different generalization from the rest of exuber's volatility
-robustness tests: it doesn't touch the innovation variance at all, but
instead allows the explosive AR coefficient itself to vary
stochastically over time, `1 + c1/T + a*u_t/sqrt(T)`, rather than the
deterministic `1 + c/T^alpha` every recursive-ADF-family statistic in
this package assumes.

`type = "ssu"` is the single recursion (start fixed at the beginning of
the sample, `SADF`'s shape); `type = "gssu"` also takes the supremum
over window starts (`GSADF`'s shape), with the paper's own minimum
window `r0 = -0.004 + 2.24/sqrt(n)`. The paper finds GSSU no more
powerful than SSU.

`union = TRUE` adds the paper's recommended union-of-rejections
procedure: `UR = max(SADF / cv_sadf, SSU / cv_ssu)` (or `GUR` with
GSADF/GSSU), compared with the published scaling constant `ur` (`gur`).
Neither SADF nor SSU dominates: SSU wins when the explosive coefficient
is genuinely stochastic, SADF when it is deterministic, and the union
stays close to the better of the two. The SADF/GSADF side is
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) with
its default minimum window and `lag = 0`, against `cv` (default: the
precomputed critical values).

## Note

The SSU/GSSU critical values and the union constants are published
asymptotic values (Kurozumi & Nishi (2025)'s Table I) – no simulation
needed. The union constant is only valid at the level the statistic was
built for.

Returns its own class (not `radf_obj`), so it does not plug into
[`summary()`](https://rdrr.io/r/base/summary.html)/`\link{datestamp}`/`tidy`;
it has its own [`print()`](https://rdrr.io/r/base/print.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods instead. Prints its own statistic/critical-value summary – see
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md)
for the full picture of which functions do and don't fit that pipeline.

## Status

**\[experimental\]**

## References

Kurozumi, E., & Nishi, M. (2025). Bubble testing with stochastically
varying explosive coefficient. Journal of Time Series Analysis, 46(5),
945-965.

## See also

[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) for
the deterministic-coefficient recursive ADF-family alternative this
complements.

Other volatility-robust tests:
[`cusum_test()`](https://kvasilopoulos.github.io/exuber/reference/cusum_test.md),
[`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md),
[`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md),
[`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md),
[`radf_sign()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md),
[`radf_sign_dm()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md),
[`radf_tt()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md)

## Examples

``` r
# \donttest{
# A stochastically varying explosive root, rho_t = 1 + 3/n + 4 * u_t / sqrt(n):
# the alternative ssu_test() is built for (a fixed-root DGP is lbi_test()'s)
y <- sim_psy1(n = 150, te = 75, tf = 150, c = 3, alpha = 1, seed = 2001,
  coef_noise = rnorm(149), coef_a = 4)
res <- ssu_test(y, sig_lvl = 95)
print(res)
#> 
#> ── ssu_test (SSU, n = 150, minw = 23, sig_lvl = 95%, crit = 3.3) ───────────────
#> 
#>    series   sadf  detected
#>   series1  15.02      TRUE
#> 

# The double-recursion version
ssu_test(y, type = "gssu")
#> 
#> ── ssu_test (GSSU, n = 150, minw = 26, sig_lvl = 95%, crit = 5.37) ─────────────
#> 
#>    series   sadf  detected
#>   series1  15.02      TRUE
#> 

# Plot the recursive SSU statistic path against its critical value
autoplot(res)

# }
```
