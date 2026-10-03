# Simulate GARCH(1,1)/TGARCH(1,1) innovations

Generates shocks `z_t = sqrt(h_t) * eps_t` from a GARCH(1,1) recursion
with an optional threshold (leverage) term, for use as
`sim_psy1(..., e = sim_vol_garch(...))`.

## Usage

``` r
sim_vol_garch(n, omega = 0.1, alpha = 0.1, beta = 0.8, gamma = 0, seed = NULL)
```

## Arguments

- n:

  Number of innovations to generate.

- omega, alpha, beta:

  Positive GARCH(1,1) parameters. The defaults
  (`omega = 0.1, alpha = 0.1, beta = 0.8`) match Whitehouse, Harvey &
  Leybourne (2025) and Harvey, Leybourne, Taylor & Zu (2024).

- gamma:

  Non-negative TGARCH leverage parameter. The NASDAQ calibration of
  Monschang & Wilfling (2021) is
  `omega = 0.4387, alpha = 0, beta = 0.9319, gamma = 0.1306`.

- seed:

  An object specifying if and how the random number generator (rng)
  should be initialized. It is either NULL or an integer, which is
  passed to `set.seed` before the simulation. If you set it, the value
  is saved as the "seed" attribute of the returned value. The default,
  NULL, leaves the state of the rng unchanged and returns .Random.seed
  as the "seed" attribute. Results are reproducible across the parallel
  and the non-parallel option when you use the same seed.

## Value

A numeric vector of length `n`.

## Details

\$\$z_t = \sqrt{h_t}\\\epsilon_t,\quad h_t = \omega + \alpha
z\_{t-1}^2 + \beta h\_{t-1} + \gamma z\_{t-1}^2 1\\z\_{t-1}\<0\\\$\$

with \\\epsilon_t \sim NIID(0,1)\\ and \\h_0 = z_0 = 0\\. `gamma = 0`
(the default) gives plain GARCH(1,1), and `gamma > 0` adds the TGARCH
leverage effect, a larger response to negative shocks.

## References

Whitehouse, E.J., Harvey, D.I. & Leybourne, S.J. (2025). "Real-time
monitoring of explosive financial bubbles." Monschang, V. & Wilfling, B.
(2021). "Sup-ADF-style bubble-detection methods under test." Empirical
Economics, 61, 145-172.

## See also

[`sim_psy1`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)

## Examples

``` r
sim_vol_garch(199, seed = 1) %>%
  autoplot()

# NASDAQ-calibrated TGARCH (Monschang & Wilfling 2021)
sim_vol_garch(199, omega = 0.4387, alpha = 0, beta = 0.9319, gamma = 0.1306, seed = 1) %>%
  autoplot()
```
