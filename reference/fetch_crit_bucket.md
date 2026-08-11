# Fetch simulated critical values for a given (n, lag), disk-cached

Checks a persistent local cache first (survives across sessions), then
the bucket proxy. Returns `NULL` (rather than erroring) if neither has
it, so callers can fail gracefully.

## Usage

``` r
fetch_crit_bucket(n, lag = 0, base_url = crit_bucket_base_url)
```
