#Simulate data
set.seed(4441)
dta <- data.frame(
  "dgp1" = sim_dgp1(100),
  "dgp2" = sim_dgp2(100),
  "evans" = sim_evans(100),
  "div"  = sim_div(100),
  "blan" = sim_blan(100)
)

<<<<<<< HEAD
# NA to generate error message
dta_na <- dta
dta_na[1, 3] <- NA

# Use main functions to compute tstats and critical values
=======
dta_df <- data.frame(
  "dgp1" = sim_dgp1(100),
  "dgp2" = sim_dgp2(100),
  "evans" = sim_evans(100),
  "div"  = sim_div(100),
  "blan" = sim_blan(100),
  "date" = seq(as.Date("2000-01-01"), by = "month", length.out = 100)
)

# NA to generate error message
dta_na <- dta
dta_na[1, 3] <- NA
#
# # Use main functions to compute tstats and critical values
>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods
radf_dta <- radf(dta)
radf_dta_lag1 <- radf(dta, lag = 1)

# # Create a series "Reject"
set.seed(1333)
radf_div <- radf(sim_div(100))

# # Create a series that do not reject at 95 sig level
set.seed(1132)
radf_95 <- radf(sim_dgp1(100, alpha = 0.66))
invisible(capture.output(mc <- mc_cv(100, 100)))

set.seed(1333)
invisible(capture.output(wb <- wb_cv(dta, 100)))
invisible(capture.output(mc2 <- mc_cv(100, 100, minw = 20)))

<<<<<<< HEAD
# Panel critical values
invisible(capture.output(sb <- panel_cv(dta, nboot = 100, lag = 0)))
invisible(capture.output(sb1 <- panel_cv(dta, nboot = 100, lag = 1)))
=======
# # Panel critical values
invisible(capture.output(sb <- sb_cv(dta, nboot = 100, lag = 0)))
invisible(capture.output(sb1 <- sb_cv(dta, nboot = 100, lag = 1)))
>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods
