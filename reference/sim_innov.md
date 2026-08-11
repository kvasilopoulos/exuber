# Simulate innovations with heavy-tailed/skewed marginal distributions

Generates a shock sequence with the same PSY-style mean equation in mind
([`sim_psy1`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md),
[`sim_psy2`](https://kvasilopoulos.github.io/exuber/reference/sim_psy2.md))
but a non-Gaussian marginal, standardized to mean 0 and variance
`sigma^2` so it drops straight into `sim_psy1(..., e = sim_innov(...))`.

## Usage

``` r
sim_innov(
  n,
  dist = c("normal", "t", "skew_t"),
  sigma = 6.79,
  df = 5,
  xi = 0,
  seed = NULL
)
```

## Arguments

- n:

  Number of innovations to generate.

- dist:

  One of `"normal"`, `"t"`, `"skew_t"`.

- sigma:

  A positive scalar indicating the standard deviation of the
  innovations.

- df:

  Degrees of freedom for `"t"`/`"skew_t"` (`> 2`).

- xi:

  Skewness parameter for `"skew_t"` (any real; 0 = symmetric).

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

`dist = "t"` rescales a Student-t(`df`) draw to variance 1 before
scaling by `sigma` (exact, closed form: `Var(t_df) = df / (df - 2)`).
`dist = "skew_t"` combines two independent standardized Student-t draws
Azzalini-style, `delta * abs(T0) + sqrt(1 - delta^2) * T1` with
`delta = xi / sqrt(1 + xi^2)`, then standardizes using the closed-form
mean/variance of that combination (via `E|T0|`, itself closed-form
through the Beta function). `xi > 0` skews right, `xi < 0` skews left,
`xi = 0` reduces to the symmetric `t` case.

## References

Wu, R., Shi, S. & Wu, J. (2025). "Quantile analysis for financial bubble
detection and surveillance." JTSA, 46(5), 908-931 (uses
N(0,1)/t(3)/skewed-t(3, -0.75)/skewed-t(3, 0.75) innovations in their
Monte Carlo design, eq. 6).

## See also

[`sim_psy1`](https://kvasilopoulos.github.io/exuber/reference/sim_psy1.md)

## Examples

``` r
sim_innov(199, dist = "skew_t", df = 3, xi = -0.75, seed = 1)
#>   [1]   1.70445110   1.41067595   8.19542155 -17.86656662   6.54936337
#>   [6]   2.26338454  -1.86399211  -5.59988832  -2.49846849   0.67160710
#>  [11]  -0.73202604  -4.71244996  -2.75029622  -0.88984120   4.27832088
#>  [16]  -0.35838575  11.23009592  -3.44270521  -0.43614695   2.15638528
#>  [21]   1.63349366   2.21654383   6.78987086   5.45879303  -7.95028858
#>  [26] -17.16700090  -0.45911468 -15.54124080   0.38528531  -0.13032808
#>  [31]   4.16345916   0.16741987   1.91988779  -0.52270153  -0.36090870
#>  [36]   1.43374614   0.59140452  -5.08094806   0.82786935   0.40995560
#>  [41]   4.08562075  13.60943953   0.08373503  -9.50975790   3.03118508
#>  [46]   4.11709288   1.36914028  -0.20463444  -0.48054441  -4.00616977
#>  [51]  -4.66337741   2.86689514   6.98990593  -2.61915934  -0.44843205
#>  [56]  -5.49479929   5.72593671  -1.00981235  -2.14921253  -3.00782150
#>  [61]   2.57560779  -6.78677388  14.87444877  -0.23456181  -1.13270733
#>  [66]  -2.92517917   2.99118268   3.53382038   5.84296018  -2.50177874
#>  [71]   3.59489237  -1.83933987   6.17702012  -4.21830773   0.17373034
#>  [76]  -2.21468381  -3.78945064  10.84567760   9.26941967   3.90785099
#>  [81]  -7.62458176 -13.57354096   8.29709978  -1.68811046   1.03999777
#>  [86]   1.84687197   2.98301719  -5.38165046  -3.34663260  -5.79330181
#>  [91]  -0.30028182  -0.81951396  -7.72120889   3.78530885   1.23809035
#>  [96]   0.03279621   7.65187158  23.82892740   1.03219923  -3.55378177
#> [101]  -6.10911474  -0.92996711   2.12558681   1.54529119   2.40724761
#> [106]   4.29502609   6.60087586   3.02109823   1.88244043   0.22240935
#> [111] -14.01888349  -5.05891752  -4.29756349  -6.97848539   3.51140857
#> [116]   2.77872520  -2.71368528  -0.64931430   3.61834189   0.35730604
#> [121] -10.41416441  -0.98021009  -1.21152190   3.21051631  -4.51849323
#> [126]  12.32008741  -8.04687133  -1.08119172  -3.29584479   6.61986187
#> [131]  -2.43663285   3.36706489   0.90676535   1.91772703  -0.94159084
#> [136]  12.33418717  -3.27353630   9.29612017   7.88899135   2.40969973
#> [141] -11.65355353   3.12792356   5.38936405 -11.55890648  -4.30384747
#> [146]  -6.21108060   1.93160146   2.44659447   0.79544741  -5.90842801
#> [151]   5.71010503  -0.39645473  -3.39049377  -5.04699171   5.11053350
#> [156]   0.98522991  -6.26796233  -2.83189531   1.63935213  -0.02987215
#> [161]  -0.37092213   4.60354099  -7.71582972   4.27437803 -12.57742786
#> [166]   2.01256724  -3.33812883   1.35492081   9.84279582  -2.83333785
#> [171]  10.67543256  -2.09865048   0.25254517   1.87153455  -0.74883151
#> [176]  -6.48845020   2.25935571   4.10029442   0.31644642   4.61282763
#> [181]   3.83974838  -1.05011026   4.18972592   2.59309023   8.10634526
#> [186]  -9.91005329  -5.55848491  -2.05937179  10.06654501  -4.38623790
#> [191]   9.85807299   2.84666698   0.97105585  -1.57340005   1.04155262
#> [196] -23.69396726  -0.97860050   4.00176813   7.58746904
```
