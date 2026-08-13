# Sign-Based Bubble Test (sPWY / sPSY)

`radf_sign` computes Harvey, Leybourne & Zu (2020)'s sign-based variant
of the recursive right-tailed unit root test: instead of applying the
(double-)supremum ADF test directly to the series, it is applied to the
cumulated sign of its first differences, `C_t = sum(sign(diff(y)))`.
Because [`sign()`](https://rdrr.io/r/base/sign.html) strips out all
magnitude information, `C_t`'s recursive DF statistic is *exactly*
invariant to the pattern of (even time-varying) volatility in the
innovations – unlike
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md), no
wild bootstrap is needed to control size under heteroskedasticity;
[`radf_sign_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md)'s
critical values are pivotal, computed once rather than per dataset.

## Usage

``` r
radf_sign(data, minw = NULL)
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

## Details

The cost of this invariance is power: the paper finds the sign-based
test outperforms the standard PSY test for many time-varying-volatility
and bubble specifications, but not all – the standard test can still win
for some. The paper's own recommended practical strategy is a
bootstrap-based union-of-rejections combining both tests, which is
**not** implemented here (see the package's enhancement notes for the
cost/benefit reasoning); this function provides the standalone
sign-based test only. `sadf` is the single-supremum (`r1 = 0` fixed)
sPWY statistic; `gsadf` is the double-supremum sPSY statistic.

## Note

Needs
[`radf_sign_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_sign_cv.md)
for critical values, not
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md)
or any other bootstrap – the statistic is pivotal (exactly invariant to
heteroskedasticity), so its critical values are simulated once, not per
dataset.

## Level-shift robustness

Harvey, Leybourne, Tatlow & Zu (2025) show this test also retains its
standard (no-level-shift) null distribution in the presence of
deterministic level shifts, provided the number of shifts grows strictly
slower than `sqrt(T)` – regardless of how large the shifts are. This is
a materially weaker requirement than the standard PSY test needs for its
own size control, which restricts the number **and** the magnitude of
shifts jointly; in their simulations the standard test is never
correctly sized once the number of shifts grows at rate `sqrt(T)`, while
this test stays close to nominal size.

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
for the recursively demeaned sign-based analogue (sharing the same
level-shift robustness), and
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md) for
the standard (non-invariant) test.

## Examples

``` r
# \donttest{
res <- radf_sign(sim_data, minw = 20)
print(res)
#> 
#> ── radf_sign (minw = 20) ───────────────────────────────────────────────────────
#> 
#>   series      adf    sadf   gsadf
#>     psy1  -0.1516  0.9367   2.021
#>     psy2   2.5578  6.4212  13.985
#>    evans   4.8486  5.7582   6.852
#>      div   1.1346  2.7920   2.950
#>     blan   3.3805  3.3805   3.684
#> 

cv <- radf_sign_cv(n = 100, minw = 20)
summary(res, cv = cv)
#> Error in full_join(tidy(x, format = "long"), tidy(y, format = "long"),     by = c("stat", join_by), relationship = "many-to-many"): Join columns in `y` must be present in the data.
#> ✖ Problem with `id`.
# }
```
