# Simulation of dividends

Simulate (log) dividends from a random walk with drift.

## Usage

``` r
sim_div(
  n,
  mu,
  sigma,
  r = 0.05,
  log = FALSE,
  output = c("pf", "d"),
  seed = NULL
)
```

## Arguments

- n:

  A positive integer specifying the length of the simulated output
  series.

- mu:

  A scalar indicating the drift.

- sigma:

  A positive scalar indicating the standard deviation of the
  innovations.

- r:

  A positive value indicating the discount factor.

- log:

  Logical. If true dividends follow a lognormal distribution.

- output:

  A character string giving the fundamental price("pf") or dividend
  series("d"). Default is `pf`.

- seed:

  An object specifying if and how the random number generator (rng)
  should be initialized. Either NULL or an integer will be used in a
  call to `set.seed` before simulation. If set, the value is saved as
  "seed" attribute of the returned value. The default, NULL, will not
  change rng state, and return .Random.seed as the "seed" attribute.
  Results are reproducible across the parallel and non-parallel option
  when the same seed is used.

## Value

A numeric vector of length n.

## Details

If log is set to FALSE (default value) dividends follow:

\$\$d_t = \mu + d\_{t-1} + \epsilon_t\$\$

where \\\epsilon \sim \mathcal{N}(0, \sigma^2)\\. The default parameters
are \\\mu = 0.0373\\, \\\sigma^2 = 0.1574\\ and \\d\[0\] = 1.3\\ (the
initial value of the dividend sequence). The above equation can be
solved to yield the fundamental price:

\$\$F_t = \mu(1+r)r^{-2} + r^{-1}d_t\$\$

If log is set to TRUE then dividends follow a lognormal distribution or
log(dividends) follow:

\$\$\ln(d_t) = \mu + \ln(d\_{t-1}) + \epsilon_t\$\$

where \\\epsilon \sim \mathcal{N}(0, \sigma^2)\\. Default parameters are
\\\mu = 0.013\\, \\\sigma^2 = 0.16\\. The fundamental price in this case
is:

\$\$F_t = \frac{1+g}{r-g}d_t\$\$

where \\1+g=\exp(\mu+\sigma^2/2)\\. All default parameter values are
those suggested by West (1988).

## References

West, K. D. (1988). Dividend innovations and stock price volatility.
Econometrica: Journal of the Econometric Society, p. 37-61.

## Examples

``` r
# Price is the sum of the bubble and fundamental components
# 20 is the scaling factor
pf <- sim_div(100, r = 0.05, output = "pf", seed = 123)
pb <- sim_evans(100, r = 0.05, seed = 123)
p <- pf + 20 * pb

autoplot(p)
```
