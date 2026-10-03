# Simulate fractionally-integrated (long-memory) innovations

Generates \\u_t = \Delta^{-d}\epsilon_t\\, with \\\epsilon_t\\ i.i.d.
\\(0, \sigma^2)\\, through a truncated \\MA(\infty)\\ expansion of the
fractional-differencing operator, for use as
`sim_psy1(..., e = sim_fi(...))`.

## Usage

``` r
sim_fi(n, d = 0.2, sigma = 1, seed = NULL)
```

## Arguments

- n:

  Number of innovations to generate.

- d:

  Long-memory (fractional differencing) parameter, in (0, 0.5) so that
  \\u_t\\ itself is stationary.

- sigma:

  A positive scalar indicating the standard deviation of the
  innovations.

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

\$\$\Delta^{-d} = \sum\_{j=0}^\infty \psi_j L^j,\quad \psi_0 = 1,\quad
\psi_j = \psi\_{j-1}\frac{j-1+d}{j}\$\$

The expansion is truncated at `max(200, n)` lags with a matching
burn-in, which is dropped before the function returns, to limit the
truncation bias in the early observations.

## References

Lui, Y.L., Phillips, P.C.B. & Yu, J. (2024). "Robust testing for
explosive behavior with strongly dependent errors."

## See also

[`sim_psy1`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)

## Examples

``` r
sim_fi(199, d = 0.2, seed = 1) %>%
  autoplot()
```
