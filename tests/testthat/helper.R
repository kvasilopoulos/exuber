# # Set options
options(exuber.show_progress = FALSE)
options(exuber.parallel = FALSE)

# Set options
# options(exuber.show_progress = TRUE)
# options(exuber.parallel = TRUE)
#
# getOption("exuber.parallel")
# getOption("exuber.show_progress")


sim_data_mat <- as.matrix(sim_data)

# not have helpers in interactive mode (take too much to load)
# if (!interactive() || identical(Sys.getenv("NOT_CRAN"), "TRUE")) {

dta <- sim_data
dta_df <- sim_data_wdate
dta_mat <- as.matrix(dta)

# NA to generate error message
dta_na <- dta
dta_na[1, 3] <- NA

suppressMessages({
  # Use main functions to compute tstats and critical values
  radf_dta <- radf(dta)
  radf_dta_lag1 <- radf(dta, lag = 1)
  radf_dta_lag2 <- radf(dta, lag = 2)
  radf_dta_wdate <- radf(dta_df)

  # Create a series "Reject"
  set.seed(1333)
  radf_div <- radf(sim_div(100))

  # Create a series that do not reject at 95 sig level
  set.seed(1132)
  radf_95 <- radf(sim_psy1(100, alpha = 0.66))
  mc <- radf_mc_cv(100, nrep = 100)
  set.seed(1333)
  wb <- radf_wb_cv(dta, nboot = 100)

  # # Panel critical values
  sb <- radf_sb_cv(dta, nboot = 100, lag = 0)
  sb1 <- radf_sb_cv(dta, nboot = 100, lag = 1)
  sb2 <- radf_sb_cv(dta, nboot = 100, lag = 2)
})


set.seed(123)
sim_ds <- tibble::tibble(
  ongoing = sim_psy1(100, te = 80, tf = 100, seed = 123),
  negative = -1*sim_ps1(100, te = 30, tf = 60, tr =  80, beta = 0.5),
  div = sim_div(100),
  positive = sim_psy1(100)
)
suppressMessages({
  radf_ds <- radf(sim_ds)
})












