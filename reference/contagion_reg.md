# Bubble Contagion Regression (Greenaway-McGrevy & Phillips 2016)

`contagion_reg` estimates the time-varying contagion coefficient of
Greenaway-McGrevy & Phillips (2016): a fixed-window rolling AR(1)
coefficient sequence for a "core" series and a "satellite" series `y`,
related by a functional (Nadaraya-Watson kernel) regression at a chosen
delay `d` – how strongly and how (time-varying) does the core series'
local persistence transmit to `y`, `d` periods later.

## Usage

``` r
contagion_reg(
  y,
  core,
  S = NULL,
  d = 0L,
  h = NULL,
  r_grid = seq(0, 1, length.out = 100)
)
```

## Arguments

- y:

  Satellite (dependent) series, numeric vector.

- core:

  Core (reference) series, numeric vector, same length as `y`.

- S:

  Fixed rolling-window width for the AR(1) coefficient sequence (default
  `floor(0.33 * length(y))`, the paper's own choice).

- d:

  Non-negative integer delay (default `0`).

- h:

  Bandwidth for the Nadaraya-Watson regression. Default `NULL` selects
  it via leave-one-out cross-validation (eq. 7).

- r_grid:

  Evaluation points for the time-varying coefficient, as fractions of
  the sample (default `seq(0, 1, length.out = 100)`).

## Value

An object of class `contagion_reg_obj`: a list with the fixed-window
AR(1) coefficient sequences (`beta_core`, `beta_j`), the
selected/supplied bandwidth (`h`), and the estimated time-varying
contagion coefficient (`delta2`, aligned with `r_grid`).

## Details

This is the minimum-viable subset of the paper's own procedure: the
fixed-window AR(1) coefficient sequence (their eq. 1), the
Nadaraya-Watson regression at a single supplied `d` (eq. 6), and
leave-one-out cross-validated bandwidth selection (eq. 7). Their eq. 8
(searching over `d` automatically) is not implemented – call
`contagion_reg` once per candidate `d` and compare fit if an automatic
search is needed.

The paper performs no formal inference (no confidence bands, no
hypothesis test) on the contagion coefficient itself – this is a
point-estimation and visualization tool, not a test, matching what the
source paper itself does.

## Note

Not a hypothesis test: `contagion_reg` performs no formal inference (no
confidence bands, no significance test) on the contagion coefficient, so
there is no critical value at all for this function – don't look for
one.

## Status

**\[experimental\]**

## References

Greenaway-McGrevy, R., & Phillips, P. C. B. (2016). Hot property in New
Zealand: Empirical evidence of housing bubbles in the metropolitan
centres. New Zealand Economic Papers, 50(1), 88-113.

## See also

[`cobubble_test`](https://kvasilopoulos.github.io/exuber/reference/cobubble_test.md)
for a different (symmetric, hypothesis-testing) bivariate bubble
relationship.

## Examples

``` r
# \donttest{
res <- contagion_reg(sim_data$sim_psy1, sim_data$sim_psy2, d = 0L)
#> Warning: Unknown or uninitialised column: `sim_psy1`.
#> Warning: Unknown or uninitialised column: `sim_psy2`.
#> Error in y[1:n1]: only 0's may be mixed with negative subscripts
print(res)
#> Error: object 'res' not found
# }
```
