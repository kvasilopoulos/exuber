options(exuber.show_progress = FALSE)
options(exuber.parallel = FALSE)

#Simulate data
set.seed(4441)
dta <- data.frame(
  "dgp1" = sim_dgp1(100),
  "dgp2" = sim_dgp2(100),
  "evans" = sim_evans(100),
  "div"  = sim_div(100),
  "blan" = sim_blan(100)
)


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
radf_dta <- radf(dta)
radf_dta_lag1 <- radf(dta, lag = 1)

# # Create a series "Reject"
set.seed(1333)
radf_div <- radf(sim_div(100))

# # Create a series that do not reject at 95 sig level
set.seed(1132)
radf_95 <- radf(sim_dgp1(100, alpha = 0.66))
invisible(capture.output(mc <- mc_cv(100, nrep = 100)))

set.seed(1333)
invisible(capture.output(wb <- wb_cv(dta, nboot = 100)))

# # Panel critical values
invisible(capture.output(sb <- sb_cv(dta, nboot = 100, lag = 0)))
invisible(capture.output(sb1 <- sb_cv(dta, nboot = 100, lag = 1)))

# Different minw
invisible(capture.output(mc2 <- mc_cv(100, nrep = 100, minw = 20)))
invisible(capture.output(wb2 <- wb_cv(dta, nboot = 100, minw = 20)))
invisible(capture.output(sb2 <- sb_cv(dta, nboot = 100, minw = 20)))
