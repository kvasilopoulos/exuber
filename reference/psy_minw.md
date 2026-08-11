# Helper functions in accordance to PSY(2015)

`psy_minw` and `psy_ds` use the rules-of- thumb proposed by Phillips et
al. (2015) to compute the minimum window size and the minimum duration
of an episode of exuberance, respectively.

## Usage

``` r
psy_minw(n)

psy_ds(n, rule = 1, delta = 1)
```

## Arguments

- n:

  A positive integer. The sample size.

- rule:

  Rule to compute the minimum duration of an episode (default: rule = 1,
  where T denotes the sample size). Rule 1 corresponds to log(T), while
  rule 2 to log(T)/T.

- delta:

  Frequency-dependent parameter (default; delta = 1). See details.

## Details

For the minimum duration period, `psy_ds` allows the user to choose from
two rules:

\$\$rule_1 = \delta \log(n) \quad\\ \quad rule_2 = \delta \log(n)/n\$\$

`delta ` depends on the frequency of the data and the minimal duration
condition.

## References

Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for Multiple
Bubbles: Historical Episodes of Exuberance and Collapse in the S&P 500.
International Economic Review, 56(4), 1043-1078.

## Examples

``` r
psy_minw(100)
#> [1] 19
psy_ds(100)
#> [1] 5
```
