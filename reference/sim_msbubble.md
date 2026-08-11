# Simulation of a Markov-switching present-value bubble

Simulation of Chan & Santi (2021)'s bubble component of a present-value
state-space model: an AR(1) whose persistence switches between a
"surviving" (explosive) and a "collapsing" (mean-reverting) regime under
a first-order Markov chain, rather than at deterministic dates
([`sim_psy1`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md))
or a fixed-probability mixture
([`sim_blan`](https://kvasilopoulos.github.io/exuber/reference/sim_blan.md)).

## Usage

``` r
sim_msbubble(
  n,
  p11 = 0.98,
  p22 = 0.9,
  lambda1 = 0.98,
  lambda2 = 1.03,
  sigma_b = 0.05,
  b0 = 0,
  s0 = 1L,
  seed = NULL
)
```

## Arguments

- n:

  A positive integer specifying the length of the simulated output
  series.

- p11, p22:

  Regime-1-to-1 and regime-2-to-2 transition probabilities, in (0, 1).

- lambda1, lambda2:

  Regime persistence parameters (`lambda1 < 1` explosive, `lambda2 > 1`
  mean-reverting).

- sigma_b:

  A positive scalar, the bubble-innovation standard deviation.

- b0:

  Starting value.

- s0:

  Starting regime, `1L` or `2L`.

- seed:

  An object specifying if and how the random number generator (rng)
  should be initialized. Either NULL or an integer will be used in a
  call to `set.seed` before simulation. If set, the value is saved as
  "seed" attribute of the returned value. The default, NULL, will not
  change rng state, and return .Random.seed as the "seed" attribute.
  Results are reproducible across the parallel and non-parallel option
  when the same seed is used.

## Value

A numeric vector of length `n`, with a `"regime"` attribute (the
simulated `S_t` path).

## Details

\$\$b_t = \frac{1}{\lambda\_{S_t}}b\_{t-1}+\epsilon_t^b,\quad
\epsilon_t^b \sim iid\\N(0,\sigma_b^2)\$\$ with \\S_t \in \\1,2\\\\ a
Markov chain with transition probabilities `p11 = P(S[t]=1|S[t-1]=1)`,
`p22 = P(S[t]=2|S[t-1]=2)`. Regime 1 ("surviving") uses `lambda1 < 1`
(so `1/lambda1 > 1`, explosive); regime 2 ("collapsing") uses
`lambda2 > 1` (mean-reverting). Note: the source's own eq. 16 indexes
the coefficient by \\S\_{t+1}\\; this implementation uses the
contemporaneous \\S_t\\ instead (an indexing-convention simplification,
not a change to the qualitative Markov-switching mechanism).

## References

Chan, J.C.C. & Santi, C. (2021). "Speculative Bubbles in Present-Value
Models: A Bayesian Markov-Switching State Space Approach." Journal of
Economic Dynamics and Control, 127, 104101.

## See also

[`sim_psy1`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md),
[`sim_blan`](https://kvasilopoulos.github.io/exuber/reference/sim_blan.md)

## Examples

``` r
sim_msbubble(200, seed = 123) %>%
  autoplot()
```
