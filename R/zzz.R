
.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  op <- options()

  ncores <- parallel::detectCores() - 1

  op.exuber <- list(
    exuber.show_progress = TRUE,
    exuber.parallel = TRUE,
    exuber.ncores = ncores,
    exuber.global_seed = NA
  )
  toset <- !(names(op.exuber) %in% names(op))
  if (any(toset)) options(op.exuber[toset])

  has_data <- requireNamespace("exuberdata", quietly = TRUE)
  .pkgenv[["has_data"]] <- has_data

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (!.pkgenv$has_data) {
    msg <- paste(
      "To use this package, you must install the",
      "exuberdata package. To install that ",
      "package, run `install.packages('exuberdata',",
      "repos='https://kvasilopoulos.github.io/drat/',",
      "type='source')`. "
    )
    packageStartupMessage(msg)
  }
}

# TODO ui customization to download

need_data <- function(has_data = .pkgenv$has_data) {
  if (!has_data) {
    msg <- paste("To use this package, you must have the exuberdata installed.")
    stop(msg)
  }
}


# Set Global Variables to avoid NOTES in cmdchecks
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c("adf", "sadf", "gsadf", "badf", "bsadf", "bsadf_panel", "gsadf_panel",
      "Distribution", ".", "crit", "i", "id", "name","sig", "key", "tstat",
      "value", "value_x", "value_y", "stat", "pval")
  )
}

# citation ----------------------------------------------------------------

# .onAttach <-
#   function(libname, pkgname) {
#     packageStartupMessage("\nPlease cite as:\n",
#                           " Vasilopoulos, Pavlidis and Spavound (2018): exuber\n",
#                           " R package version 0.1.0. https://CRAN.R-project.org/package=exuber \n")
#   }
