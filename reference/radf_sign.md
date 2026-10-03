# Sign-Based Bubble Test (sPWY / sPSY)

`radf_sign` computes the sign-based variant of the recursive
right-tailed unit root test of Harvey, Leybourne & Zu (2020). Instead of
applying the (double-)supremum ADF test to the series itself, it applies
it to the cumulated sign of the first differences,
`C_t = sum(sign(diff(y)))`. The
[`sign()`](https://rdrr.io/r/base/sign.html) function removes all
information about magnitudes, so the recursive DF statistic of `C_t` is
*exactly* invariant to the pattern of volatility in the innovations,
even when volatility changes over time. Unlike
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md), the
test needs no wild bootstrap to control its size under
heteroskedasticity. The critical values in
[`radf_sign_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md)
are pivotal, so they are computed once and not for each dataset.

## Usage

``` r
radf_sign(data, minw = NULL)
```

## Arguments

- data:

  A univariate or multivariate numeric time series object, a numeric
  vector or matrix, or a data.frame. A column may have leading or
  trailing `NA` values, which describes an unbalanced panel in which
  series enter or exit the sample at different times. Those periods are
  filled with `NA` in `badf` and `bsadf` and excluded from the `adf`,
  `sadf` and `gsadf` of that series. Interior `NA` values (a gap in the
  middle of a series) are not supported. When any series is padded in
  this way, the panel statistics (`bsadf_panel` and `gsadf_panel`) are
  not available, and the function returns `NA` for them with a warning.

- minw:

  A positive integer. The minimum window size (default = \\(0.01 +
  1.8/\sqrt{T})T\\, where T denotes the sample size).

## Value

An object of class `radf_sign_obj`/`radf_obj`. It is the same
`adf`/`badf`/`sadf`/`bsadf`/`gsadf` list as for
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md),
computed on the sign-transformed series, and it pairs with
[`radf_sign_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md).

## Details

The price of this invariance is power. The paper finds that the
sign-based test outperforms the standard PSY test for many
specifications of time-varying volatility and bubbles, but not for all
of them, and the standard test can still win in some. The strategy that
the paper recommends in practice is a bootstrap-based union of
rejections that combines both tests. We have not implemented it (see the
package's enhancement notes for the cost and benefit considerations),
and this function provides the standalone sign-based test only. `sadf`
is the single-supremum sPWY statistic (`r1 = 0` fixed), and `gsadf` is
the double-supremum sPSY statistic.

## Note

The test needs the critical values from
[`radf_sign_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md),
and neither
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
nor any other bootstrap applies. The statistic is pivotal (exactly
invariant to heteroskedasticity), so its critical values are simulated
once and not for each dataset.

The result carries the `radf_obj` class. Since 2026-08-18 the full
[`summary()`](https://rdrr.io/r/base/summary.html),
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md),
`tidy` and `autoplot` pipeline works, because
[`radf_sign_cv()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md)
now computes the time-varying `badf_cv` and `bsadf_cv` boundary that the
last two need, and not only the three scalar critical values that
[`summary()`](https://rdrr.io/r/base/summary.html) uses. See
[`vignette("naming-and-analysis", package = "exuber")`](https://kvasilopoulos.github.io/exuber/articles/naming-and-analysis.md).

## Level-shift robustness

Harvey, Leybourne, Tatlow & Zu (2025) show that this test keeps its
standard null distribution, the one without level shifts, in the
presence of deterministic level shifts, provided that the number of
shifts grows strictly more slowly than `sqrt(T)`. The size of the shifts
does not matter. This is a materially weaker requirement than the one
the standard PSY test needs for size control, which restricts the number
**and** the magnitude of the shifts jointly. In their simulations the
standard test is never correctly sized once the number of shifts grows
at rate `sqrt(T)`, while this test stays close to its nominal size.

## Status

**\[experimental\]**

## References

Harvey, D. I., Leybourne, S. J., & Zu, Y. (2020). Sign-based unit root
tests for explosive financial bubbles in the presence of
deterministically time-varying volatility. Econometric Theory, 36(1),
122-169.

Harvey, D. I., Leybourne, S. J., Tatlow, D., & Zu, Y. (2025). Unit root
tests for explosive financial bubbles in the presence of deterministic
level shifts. Oxford Bulletin of Economics and Statistics, 87(5),
879-901. [doi:10.1111/obes.12668](https://doi.org/10.1111/obes.12668)

## See also

[`radf_sign_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md)
for critical values,
[`radf_sign_dm`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md)
for the recursively demeaned sign-based analogue, which has the same
level-shift robustness, and
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) for
the standard test, which is not invariant.

Other volatility-robust tests:
[`cusum_test()`](https://kvasilopoulos.github.io/exuber/reference/cusum_test.md),
[`radf_kp()`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md),
[`radf_sbz()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md),
[`radf_sbz_union()`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz_union.md),
[`radf_sign_dm()`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_dm.md),
[`radf_tt()`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md),
[`ssu_test()`](https://kvasilopoulos.github.io/exuber/reference/ssu_test.md)

## Examples

``` r
# \donttest{
# Volatility triples half-way through the sample. This is the case of
# non-stationary volatility that this test is built for, and plain radf()
# over-rejects here
y <- sim_psy1(n = 200, seed = 1, e = sim_vol_break(199))
res <- radf_sign(y, minw = 20)
print(res)
#> 
#> ── radf_sign (minw = 20) ───────────────────────────────────────────────────────
#> 
#>    series      adf   sadf  gsadf
#>   series1  -0.2933  4.468  8.879
#> 

cv <- radf_sign_cv(n = 200, minw = 20)
summary(res, cv = cv)
#> 
#> ── Summary (minw = 20, lag = 0) ──────────────── Sign-Based MC (nboot = 2000) ──
#> 
#> series1 :
#> # A tibble: 3 × 5
#>   stat   tstat  `90`  `95`  `99`
#>   <fct>  <dbl> <dbl> <dbl> <dbl>
#> 1 adf   -0.293 0.855  1.32  2.06
#> 2 sadf   4.47  2.34   2.70  3.43
#> 3 gsadf  8.88  3.51   3.91  4.92
#> 
tidy(res, cv = cv)
#> # A tibble: 1 × 4
#>   id         adf  sadf gsadf
#>   <fct>    <dbl> <dbl> <dbl>
#> 1 series1 -0.293  4.47  8.88
datestamp(res, cv = cv)
#> 
#> ── Datestamp (min_duration = 0) ─────────────────────────────── Sign-Based MC ──
#> 
#> series1 :
#>   Start Peak End Duration   Signal Ongoing
#> 1    84   84  85        1 positive   FALSE
#> 2    87  103 122       35 positive   FALSE
#> 3   123  123 124        1 positive   FALSE
#> 
autoplot(res, cv = cv)

# }
```
