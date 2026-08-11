# Simulation of a single-bubble process with multiple forms of collapse regime

The new generating process considered here differs from the `sim_psy1`
model in three respects - Phillips and Shi (2018):

*First, it includes an asymptotically negligible drift in the martingale
path during normal periods. Second, the collapse process is modeled
directly as a transient mildly integrated process that covers an
explicit period of market collapse. Third, a market recovery date is
introduced to capture the return to normal market behavior.*

- `sudden:` with `beta = 0.1` and `tr = tf + 0.01*n`

- `disturbing:` with `beta = 0.5` and `tr = tf + 0.1*n`

- `smooth:` with `beta = 0.9` and `tr = tf + 0.2*n`

In order to provide the duration of the collapse period `tr` as
`tr = tf + 0.2n`, you have to provide `tf` as well.

## Usage

``` r
sim_ps1(
  n,
  te = 0.4 * n,
  tf = te + 0.2 * n,
  tr = tf + 0.1 * n,
  c = 1,
  c1 = 1,
  c2 = 1,
  eta = 0.6,
  alpha = 0.6,
  beta = 0.5,
  sigma = 6.79,
  seed = NULL
)
```

## Arguments

- n:

  A positive integer specifying the length of the simulated output
  series.

- te:

  A scalar in (0, tf) specifying the observation in which the bubble
  originates.

- tf:

  A scalar in (te, n) specifying the observation in which the bubble
  collapses.

- tr:

  A scalar in (tf, n) specifying the observation in which market
  recovers

- c:

  A positive scalar determining the drift in the normal market periods.

- c1:

  A positive scalar determining the autoregressive coefficient in the
  explosive regime.

- c2:

  A positive scalar determining the autoregressive coefficient in the
  collapse regime.

- eta:

  A positive scalar (\>0.5) determining the drift in the normal market
  periods.

- alpha:

  A positive scalar in (0, 1) determining the autoregressive coefficient
  in the bubble period.

- beta:

  A positive scalar in (0, 1) determining the autoregressive coefficient
  in the collapse period.

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

Phillips, Peter CB, and Shu-Ping Shi. "Financial bubble implosion and
reverse regression." Econometric Theory 34.4 (2018): 705-753.

## See also

[`sim_psy1`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)

## Examples

``` r
# Disturbing collapse (default)
disturbing <- sim_ps1(100)
autoplot(disturbing)


# Sudden collapse
sudden <- sim_ps1(100, te = 40, tf= 60, tr = 61, beta = 0.1)
autoplot(sudden)

```
