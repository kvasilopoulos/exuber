# Simulated dataset

An artificial dataset containing series simulated from data generating
processes widely used in the literature on speculative bubbles.

## Usage

``` r
sim_data

sim_data_wdate
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 100
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 100
rows and 6 columns.

## See also

sim_psy1 sim_psy1 sim_evans sim_div sim_blan

## Examples

``` r
if (FALSE) { # \dontrun{
# The dataset can be easily replicated with the code below
library(tibble)
set.seed(1122)
sim_data <- tibble(
  sim_psy1 = sim_psy1(100),
  sim_psy2 = sim_psy2(100),
  sim_evans = sim_evans(100),
  sim_div = sim_div(100),
  sim_blan = sim_blan(100)
)
sim_data_wdate <- tibble(
  psy1 = sim_psy1(100),
  psy2 = sim_psy2(100),
  evans = sim_evans(100),
  div = sim_div(100),
  blan = sim_blan(100),
  date = seq(as.Date("2000-01-01"), by = "month", length.out = 100)
)
} # }
```
