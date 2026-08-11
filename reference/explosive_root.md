# Estimate the Explosive Autoregressive Root over a Sub-Sample

Fits the no-intercept AR(1) regression \\y_t = \rho y\_{t-1} +
\epsilon_t\\ over the sub-sample `from:to` of `data` – the model used by
Phillips & Magdalinos (2007) and Guo, Sun & Wang (2019) for inference on
a (moderately) explosive root, e.g. an episode already identified by
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md).
No intercept is included, following Phillips & Magdalinos's model (their
eq. 58 excludes it "to exclude the presence of a deterministically
explosive component").

## Usage

``` r
explosive_root(data, from, to)
```

## Arguments

- data:

  A numeric vector (a single series).

- from, to:

  Integer row positions delimiting the sub-sample (e.g. from
  [`datestamp()`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)'s
  `Start`/`End`, converted to row positions if they are dates:
  `match(start_date, index(x))`).

## Value

A list with `rho` (the OLS estimate), `se` (its standard error),
`t_stat`, and `n` (sub-sample size).

## Status

**\[experimental\]**

## References

Phillips, P. C. B., & Magdalinos, T. (2007). Limit theory for moderate
deviations from a unit root. Journal of Econometrics, 136(1), 115-130.

Guo, G., Sun, Y., & Wang, S. (2019). Testing for moderate explosiveness.
The Econometrics Journal, 22(3), 279-303.

## See also

[`root_ci`](https://kvasilopoulos.github.io/exuber/reference/root_ci.md)
for a confidence interval and doubling time based on this estimate.
