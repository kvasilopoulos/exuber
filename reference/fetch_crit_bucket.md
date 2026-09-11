# Fetch simulated critical values for a given (n, lag), disk-cached

Checks a persistent local cache first (survives across sessions), then
the bucket proxy. Returns `NULL` when the store answers 404 (that
combination hasn't been simulated yet) so callers can fail gracefully;
any other failure (no network, proxy down) is an error, since retrying
later may well succeed.

## Usage

``` r
fetch_crit_bucket(n, lag = 0, base_url = crit_bucket_base_url)
```
