# Critical Values for the Common-Bubble (PCA + PSY) Test

`radf_common_cv` simulates critical values for
[`radf_common`](https://kvasilopoulos.github.io/exuber/reference/radf_common.md)
under its own null (no common explosive factor): an `N`-column panel of
*independent* random walks, extracted to one principal component and
tested exactly as
[`radf_common`](https://kvasilopoulos.github.io/exuber/reference/radf_common.md)
does. Unlike
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
– which has no dependence on panel width and was shown by independent
validation to be badly undersized as a stand-in for `radf_common`'s own
null once `N` grows past a handful of series – this null distribution
does depend on `N`, so `N` must match the panel
[`radf_common`](https://kvasilopoulos.github.io/exuber/reference/radf_common.md)
was actually run on.

## Usage

``` r
radf_common_cv(n, N, minw = NULL, nrep = 1000L, seed = NULL)
```

## Arguments

- n:

  A positive integer. The sample size (number of time periods).

- N:

  A positive integer, at least 2. The panel width (number of series)
  that
  [`radf_common`](https://kvasilopoulos.github.io/exuber/reference/radf_common.md)
  will be run on – the critical value depends on this, unlike
  [`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md).

- minw:

  A positive integer. The minimum window size (default = \\(0.01 +
  1.8/\sqrt(T))T\\, where T denotes the sample size).

- nrep:

  A positive integer. The number of Monte Carlo simulations.

- seed:

  An object specifying if and how the random number generator (rng)
  should be initialized. Either NULL or an integer will be used in a
  call to `set.seed` before simulation. If set, the value is saved as
  "seed" attribute of the returned value. The default, NULL, will not
  change rng state, and return .Random.seed as the "seed" attribute.
  Results are reproducible across the parallel and non-parallel option
  when the same seed is used.

## Value

A list with `adf_cv`, `sadf_cv`, `gsadf_cv`, `badf_cv`, `bsadf_cv` – the
same shape as
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)'s
return value, so it can be used as a drop-in `cv` argument for
[`datestamp`](https://kvasilopoulos.github.io/exuber/reference/datestamp.md)/`tidy`/`autoplot`
on a
[`radf_common`](https://kvasilopoulos.github.io/exuber/reference/radf_common.md)
result.

## Status

**\[experimental\]**

## References

Chen, Y., Phillips, P. C. B., & Shi, S. (2023). Common Bubble Detection
in Large Dimensional Financial Systems. Journal of Financial
Econometrics, 21(4), 989-1063.

## See also

[`radf_common`](https://kvasilopoulos.github.io/exuber/reference/radf_common.md),
[`radf_mc_cv`](https://kvasilopoulos.github.io/exuber/reference/radf_mc_cv.md)
