library(exuber)

set.seed(1234)
sim_data <- tibble(
  sim_psy1 = sim_psy1(100),
  sim_psy2 = sim_psy2(100),
  sim_evans = sim_evans(100),
  sim_div = sim_div(100),
  sim_blan = sim_blan(100)
)
usethis::use_data(sim_data, overwrite = TRUE , compress = "xz")
