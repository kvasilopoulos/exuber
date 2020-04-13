
.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  op <- options()

  ncores <- parallel::detectCores() - 1

  op.exuber <- list(
    exuber.show_progress = if (interactive()) TRUE else FALSE,
    exuber.parallel = TRUE,
    exuber.ncores = ncores,
    exuber.global_seed = NA
  )
  toset <- !(names(op.exuber) %in% names(op))
  if (any(toset)) options(op.exuber[toset])

  has_exuberdata <- requireNamespace("exuberdata", quietly = TRUE)
  .pkgenv[["has_exuberdata"]] <- has_exuberdata

  invisible()
}

# .onAttach <- function(libname, pkgname) {
#   if (!has_data()) {
#     msg <- paste(
#       "To use this package more efficiently, you need to install",
#       "the {exuberdata} package which contains simulated critical values.",
#       "To install run `install_exuberdata()"
#     )
#     packageStartupMessage(msg)
#   }
# }

#' Convenience function to install exuberdata package
#'
#' This function wraps the \code{install.packages} function and offers a faster
#' and more convenient way to install exuberdata.
#'
#' @importFrom utils install.packages
#' @export
install_exuberdata <- function() {
    install.packages(
      'exuberdata',
      repos = 'https://kvasilopoulos.github.io/drat/',
      type = 'source')
}

has_exuberdata <- function() {
  .pkgenv$has_exuberdata
}

need_exuberdata <- function() {
  if (!has_exuberdata()) {
    stop_glue(
      "To use stored simulated critical values that exceed 600",
      " observations you must have `exuberdata` installed. ",
      " To install run 'install_exuberdata()'.")
  }
}

# Set Global Variables to avoid NOTES in cmdchecks
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c("adf", "sadf", "gsadf", "badf", "bsadf", "bsadf_panel", "gsadf_panel",
      "Distribution", ".", "crit", "i", "id", "name","sig", "key", "tstat",
      "value", "value_x", "value_y", "stat", "pval", "Duration")
  )
}

# citation ----------------------------------------------------------------

# .onAttach <-
#   function(libname, pkgname) {
#     packageStartupMessage("\nPlease cite as:\n",
#                           " Vasilopoulos, Pavlidis and Spavound (2018): exuber\n",
#                           " R package version 0.1.0. https://CRAN.R-project.org/package=exuber \n")
#   }
