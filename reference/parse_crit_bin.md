# Parse the fixed little-endian binary layout written by data-raw/crit-bucket/simulate-and-upload.R: int32 x4: n, minw, lag, nrows float64 x3: adf_cv, x3: sadf_cv, x3: gsadf_cv float64 x(nrows\*3): bsadf_cv (row-major) badf_cv isn't stored – it's always the constant PWY asymptotic tiling, reconstructed here instead of transferred.

Parse the fixed little-endian binary layout written by
data-raw/crit-bucket/simulate-and-upload.R: int32 x4: n, minw, lag,
nrows float64 x3: adf_cv, x3: sadf_cv, x3: gsadf_cv float64 x(nrows\*3):
bsadf_cv (row-major) badf_cv isn't stored – it's always the constant PWY
asymptotic tiling, reconstructed here instead of transferred.

## Usage

``` r
parse_crit_bin(path)
```
