
.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  op <- options()

  mc_cores <- Sys.getenv("MC_CORES") # CRAN env uses only 2 cores
  sys_cores <- if (interactive()) parallel::detectCores() - 1 else 2
  # not possible to use more than system cores
  ncores <- if (mc_cores == "") sys_cores else min(as.numeric(mc_cores), sys_cores)

  # TODO exuber.display
  op.exuber <- list(
    exuber.show_progress = TRUE,
    exuber.parallel = TRUE,
    exuber.ncores = ncores,
    exuber.global_seed = NA
  )
  toset <- !(names(op.exuber) %in% names(op))
  if (any(toset)) options(op.exuber[toset])

  invisible(NULL)
}

# Set Global Variables to avoid NOTES in cmdchecks
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c("adf", "sadf", "gsadf", "badf", "bsadf", "bsadf_panel", "gsadf_panel",
      "Distribution", ".", "crit", "i", "id", "name","sig", "key", "tstat",
      "value", "value_x", "value_y", "stat", "pval", "panel", "End","Duration",
      "ds_lgl", "rev_badf", "rev_bsadf", "rev_bsadf_panel", "tstat_crit", "data",
      "Signal", "Ongoing", "Start")
  )
}


# citation ----------------------------------------------------------------

# .onAttach <-
#   function(libname, pkgname) {
#     packageStartupMessage("\nPlease cite as:\n",
#                           " Vasilopoulos, Pavlidis and Spavound (2018): exuber\n",
#                           " R package version 0.1.0. https://CRAN.R-project.org/package=exuber \n")
#   }
