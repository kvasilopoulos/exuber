# Simulation of a mixed causal-noncausal AR(1,1) bubble

Simulates the mixed causal-noncausal autoregressive (MAR) bubble process
of Blasques, Koopman, Mingoli & Telg (2025). Transient, self-terminating
local bubbles arise on their own from the *noncausal* (forward-looking)
component. Unlike
[`sim_psy1`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md),
no origination or collapse dates are scripted.

## Usage

``` r
sim_mar(
  n,
  phi1 = 0.7,
  psi1 = 0.7,
  dist = c("cauchy", "t"),
  df = 2,
  burn = 100,
  seed = NULL
)
```

## Arguments

- n:

  A positive integer specifying the length of the simulated output
  series.

- phi1:

  Causal AR coefficient, in (0, 1).

- psi1:

  Noncausal AR coefficient, in (0, 1).

- dist:

  Innovation distribution: `"cauchy"` or `"t"` (with `df` degrees of
  freedom).

- df:

  Degrees of freedom if `dist = "t"`.

- burn:

  Non-negative burn-in length applied at *both* ends (see Details).

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

\$\$(1-\phi_1 L)(1-\psi_1 L^{-1})y_t = \epsilon_t\$\$

The function uses the standard two-sided filtering method for MAR
processes (Lanne & Saikkonen 2011; Gourieroux & Zakoian 2017). It
generates the noncausal component by running \\u_t=\psi_1
u\_{t+1}+\epsilon_t\\ *backward* from a zero boundary `burn`
observations past the end of the sample. It then generates the causal
component by running \\y_t=\phi_1 y\_{t-1}+u_t\\ *forward* from a zero
boundary `burn` observations before the start. Both burn-in windows are
then dropped.

## References

Blasques, F., Koopman, S.J., Mingoli, G. & Telg, S. (2025). "A Novel
Test for the Presence of Local Explosive Dynamics." JTSA, 46(5),
966-980.

## See also

[`sim_psy1`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)

## Examples

``` r
sim_mar(200, seed = 123) %>%
  autoplot()
```
