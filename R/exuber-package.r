#' @section Package options:
#'\code{exuber.show_progress}
#' - Should lengthy operations such as \code{radf_mc_cv()} show a progress bar? Default: TRUE
#'
#'\code{exuber.parallel}
#' - Should lengthy operations use parallel computation? Default: TRUE
#'
#'\code{exuber.ncores}
#' - How many cores to use for parallel computation. Default: system cores - 1
#'
#'\code{exuber.global_seed}
#' - When chosen automatically feeds into all functions with random-number generation. Default: NA
#'
#' @name exuber
#' @useDynLib exuber, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @docType package
#' @keywords internal
"_PACKAGE"
