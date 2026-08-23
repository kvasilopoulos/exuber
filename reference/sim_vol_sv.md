# Simulate AR(1) lognormal stochastic-volatility innovations

Generates shocks `z_t = sigma_t * eps_t` with a persistent AR(1)
log-variance, for use as `sim_psy1(..., e = sim_vol_sv(...))`.

## Usage

``` r
sim_vol_sv(n, phi = 0.98, tau = 0.1, log_sigma0_sq = 0, seed = NULL)
```

## Arguments

- n:

  Number of innovations to generate.

- phi:

  AR(1) log-variance persistence, in (0, 1).

- tau:

  Positive standard deviation of the log-variance innovations.

- log_sigma0_sq:

  Starting value of `log(sigma^2)`. Defaults to 0.

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

## Details

\$\$\log\sigma_t^2 = \phi\log\sigma\_{t-1}^2 + \eta_t,\quad \eta_t \sim
iid\\ N(0, \tau^2)\$\$

with `phi` close to (but below) 1 for the "double local-to-unity"
near-integrated-variance case studied in the source.

## References

Sarkar, A. & Wells, M.T. (2025). "Double Local-to-Unity."
arXiv:2512.06823.

## See also

[`sim_psy1`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md),
[`sim_vol_cir`](https://kvasilopoulos.github.io/exuber/reference/sim_vol_cir.md)

## Examples

``` r
sim_vol_sv(199, seed = 1) %>%
  autoplot()
```
