
.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  op <- options()

  mc_cores <- Sys.getenv("MC_CORES") # CRAN env uses only 2 cores
  sys_cores <- if (interactive()) parallel::detectCores() - 1 else 2
  # not possible to use more than system cores
  ncores <- if (mc_cores == "") sys_cores else min(mc_cores, sys_cores)

  op.exuber <- list(
    exuber.show_progress = TRUE,
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

#' Install `exuberdata` Package
#'
#' This function wraps the \code{install.packages} function and offers a faster
#' and more convenient way to install `exuberdata.`
#'
#' @importFrom utils install.packages
#' @export
#' @examples
#' \donttest{
#' if("exuberdata" %in% loadedNamespaces()) {
#'  exuberdata::radf_crit2
#' }
#' }
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
      "value", "value_x", "value_y", "stat", "pval", "panel", "Duration",
      "ds_lgl")
  )
}


# use exuberdata ----------------------------------------------------------

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

# citation ----------------------------------------------------------------

# .onAttach <-
#   function(libname, pkgname) {
#     packageStartupMessage("\nPlease cite as:\n",
#                           " Vasilopoulos, Pavlidis and Spavound (2018): exuber\n",
#                           " R package version 0.1.0. https://CRAN.R-project.org/package=exuber \n")
#   }
