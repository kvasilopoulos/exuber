library(exuber)
library(dplyr)

# sim-data ----------------------------------------------------------------

set.seed(1122)
sim_data <- tibble(
  psy1 = sim_psy1(100),
  psy2 = sim_psy2(100),
  evans = sim_evans(100),
  div = sim_div(100),
  blan = sim_blan(100)
)

usethis::use_data(sim_data, overwrite = TRUE , compress = "xz")

# sim-date-wdate ----------------------------------------------------------

sim_data_wdate <- tibble(
  sim_data,
  date = seq(as.Date("2000-01-01"), by = "month", length.out = 100)
)

usethis::use_data(sim_data_wdate, overwrite = TRUE , compress = "xz")

