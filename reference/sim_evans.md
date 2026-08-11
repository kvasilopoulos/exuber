# Simulation of an Evans (1991) bubble process

Simulation of an Evans (1991) rational periodically collapsing bubble
process.

## Usage

``` r
sim_evans(
  n,
  alpha = 1,
  delta = 0.5,
  tau = 0.05,
  pi = 0.7,
  r = 0.05,
  b1 = delta,
  seed = NULL
)
```

## Arguments

- n:

  A positive integer specifying the length of the simulated output
  series.

- alpha:

  A positive scalar, with restrictions (see details).

- delta:

  A positive scalar, with restrictions (see details).

- tau:

  The standard deviation of the innovations.

- pi:

  A positive value in (0, 1) which governs the probability of the bubble
  continuing to grow.

- r:

  A positive scalar that determines the growth rate of the bubble
  process.

- b1:

  A positive scalar, the initial value of the series. Defaults to
  `delta`.

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

`delta` and `alpha` are positive parameters which satisfy \\0 \< \delta
\< (1+r)\alpha\\. `delta` represents the size of the bubble after
collapse. The default value of `r` is 0.05. The function checks whether
`alpha` and `delta` satisfy this condition and will return an error if
not.

The Evans bubble has two regimes. If \\B_t \leq \alpha\\ the bubble
grows at an average rate of \\1 + r\\:

\$\$B\_{t+1} = (1+r) B_t u\_{t+1},\$\$

When \\B_t \> \alpha\\ the bubble expands at the increased rate of
\\(1+r)\pi^{-1}\\:

\$\$B\_{t+1} = \[\delta + (1+r)\pi^{-1} \theta\_{t+1}(B_t -
(1+r)^{-1}\delta B_t )\]u\_{t+1},\$\$

where \\\theta\\ theta is a binary variable that takes the value 0 with
probability \\1-\pi\\ and 1 with probability \\\pi\\. In the second
phase, there is a (\\1-\pi\\) probability of the bubble process
collapsing to `delta`. By modifying the values of `delta`, `alpha` and
`pi` the user can change the frequency at which bubbles appear, the mean
duration of a bubble before collapse and the scale of the bubble.

## References

Evans, G. W. (1991). Pitfalls in testing for explosive bubbles in asset
prices. The American Economic Review, 81(4), 922-930.

## See also

[`sim_psy1`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md),
[`sim_psy2`](https://kvasilopoulos.github.io/exuber/reference/sim_psy2.md),
[`sim_blan`](https://kvasilopoulos.github.io/exuber/reference/sim_blan.md)

## Examples

``` r
sim_evans(100, seed = 123) %>%
  autoplot()
```
