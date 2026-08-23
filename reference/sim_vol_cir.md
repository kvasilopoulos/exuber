# Simulate CIR-type stochastic-volatility innovations

Generates shocks driven by a Cox-Ingersoll-Ross (square-root) stochastic
variance process, Euler-Maruyama discretized, for use as
`sim_psy1(..., e = sim_vol_cir(...))`.

## Usage

``` r
sim_vol_cir(
  n,
  kappa = 0.03,
  theta = 0.25,
  xi = 0.1,
  sigma0_sq = theta,
  seed = NULL
)
```

## Arguments

- n:

  Number of innovations to generate.

- kappa, theta, xi:

  Positive CIR parameters (mean-reversion speed, long-run variance,
  vol-of-vol).

- sigma0_sq:

  Non-negative starting variance. Defaults to `theta`.

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

\$\$d\sigma^2(r) = \kappa(\theta - \sigma^2(r))dr +
\xi\sigma(r)dB(r)\$\$

discretized over `n` steps of `r` in \\\[0, 1\]\\, with variance
reflected at zero if a step would take it negative. Default parameters
(\\\kappa=0.03\\, \\\theta=0.25\\, \\\xi=0.1\\) match Harvey, Leybourne
& Zu (2019)'s robustness design, "representative of Bollerslev and Zhou
(2002)".

## References

Harvey, D.I., Leybourne, S.J. & Zu, Y. (2019). "Testing explosive
bubbles with time-varying volatility." Econometric Reviews, 38(10),
1131-1151.

## See also

[`sim_psy1`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md),
[`sim_vol_sv`](https://kvasilopoulos.github.io/exuber/reference/sim_vol_sv.md)

## Examples

``` r
sim_vol_cir(199, seed = 1) %>%
  autoplot()
```
