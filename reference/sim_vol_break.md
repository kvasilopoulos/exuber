# Simulate innovations with a permanent volatility break

Generates i.i.d. Gaussian shocks whose standard deviation shifts
permanently from `sigma` to `sigma * ratio` at observation `tau * n`,
for use as `sim_psy1(..., e = sim_vol_break(...))`. This is the
*non-stationary* volatility DGP (Cavaliere & Taylor 2007's single break)
under which
[`radf`](https://kvasilopoulos.github.io/exuber/reference/radf.md)'s
standard critical values lose size control, and the one the
volatility-robust tests
([`radf_tt`](https://kvasilopoulos.github.io/exuber/reference/radf_tt.md),
[`radf_kp`](https://kvasilopoulos.github.io/exuber/reference/radf_kp.md),
[`radf_sbz`](https://kvasilopoulos.github.io/exuber/reference/radf_sbz.md),
[`radf_sign`](https://kvasilopoulos.github.io/exuber/reference/radf_sign.md),
[`radf_wb_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_wb_cv.md))
are designed for – unlike stationary conditional heteroskedasticity
([`sim_vol_garch`](https://kvasilopoulos.github.io/exuber/reference/sim_vol_garch.md)),
whose variance profile is asymptotically flat.

## Usage

``` r
sim_vol_break(n, tau = 0.5, ratio = 3, sigma = 6.79, seed = NULL)
```

## Arguments

- n:

  Number of innovations to generate.

- tau:

  Break fraction in (0, 1): the shift happens after observation
  `floor(tau * n)`.

- ratio:

  Positive post-/pre-break standard deviation ratio; `ratio > 1` is an
  upward break (the case where
  [`radf()`](https://kvasilopoulos.github.io/exuber/reference/radf.md)
  over-rejects most), `ratio < 1` a downward one.

- sigma:

  A positive scalar indicating the standard deviation of the
  innovations.

- seed:

  An object specifying if and how the random number generator (rng)
  should be initialized. Either NULL or an integer will be used in a
  call to `set.seed` before simulation. If set, the value is saved as
  "seed" attribute of the returned value. The default, NULL, will not
  change rng state, and return .Random.seed as the "seed" attribute.
  Results are reproducible across the parallel and non-parallel option
  when the same seed is used.

## Value

A numeric vector of length `n`.

## References

Cavaliere, G. & Taylor, A.M.R. (2007). "Testing for unit roots in time
series models with non-stationary volatility." Journal of Econometrics,
140, 919-947. Harvey, D.I., Leybourne, S.J., Sollis, R. & Taylor, A.M.R.
(2016). "Tests for explosive financial bubbles in the presence of
non-stationary volatility." Journal of Empirical Finance, 38, 548-574.

## See also

[`sim_psy1`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md),
[`sim_vol_garch`](https://kvasilopoulos.github.io/exuber/reference/sim_vol_garch.md)

## Examples

``` r
sim_vol_break(199, seed = 1) %>%
  autoplot()

# Volatility triples half-way through a PSY bubble series
sim_psy1(n = 200, seed = 123, e = sim_vol_break(199, seed = 123)) %>%
  autoplot()
```
