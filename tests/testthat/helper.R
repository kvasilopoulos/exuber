# Simulate data
set.seed(4441)
dta <- cbind(
  sim_dgp1(100), sim_dgp2(100), sim_evans(100),
  sim_div(100), sim_blan(100)
)

# Use main functions to compute tstats and critical values
radf_dta <- radf(dta)
radf_dta_lag1 <- radf(dta, lag = 1)

# Create a series "Reject"
set.seed(1333)
radf_div <- radf(sim_div(100))

# Create a series that do not reject at 95 sig level
set.seed(1132)
radf_95 <- radf(sim_dgp1(100, alpha = 0.66))

invisible(capture.output(mc <- mc_cv(100, 100)))

set.seed(1333)
invisible(capture.output(wb <- wb_cv(dta, 100)))

invisible(capture.output(mc2 <- mc_cv(100, 100, minw = 20)))
