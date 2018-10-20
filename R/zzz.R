.onLoad <- function(libname, pkgname) {
  op <- options()

  ncores <- parallel::detectCores() - 1

  op.exuber <- list(
    exuber.show_progress = TRUE,
    exuber.parallel = TRUE,
    exuber.ncores = ncores
  )
  toset <- !(names(op.exuber) %in% names(op))
  if (any(toset)) options(op.exuber[toset])

  invisible()
}

