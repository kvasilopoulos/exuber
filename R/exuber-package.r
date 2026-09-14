#' @section Package options:
#'\code{exuber.show_progress}
#' - Should lengthy operations such as \code{radf_mc_cv()} show a progress bar? Default: TRUE
#'
#'\code{exuber.parallel}
#' - Should lengthy operations use parallel computation? Default: TRUE.
#'   Honored by the \code{radf_*_cv()}/\code{radf_*_distr()} simulation engines
#'   (\code{radf_mc_cv()}, \code{radf_wb_cv()}, \code{radf_sb_cv()},
#'   \code{radf_recovery_cv()}, \code{radf_common_cv()}); the standalone
#'   tests and monitors run serially regardless.
#'
#'\code{exuber.ncores}
#' - How many cores to use for parallel computation. Default: system cores - 1
#'   (2 in a non-interactive session), capped by the \code{MC_CORES}
#'   environment variable when it is set.
#'
#'\code{exuber.global_seed}
#' - When chosen automatically feeds into all functions with random-number generation. Default: NA
#'
#' @name exuber
#' @useDynLib exuber, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom lifecycle badge
#' @importFrom stats setNames approx qcauchy lm.fit
#' @docType package
#' @keywords internal
"_PACKAGE"
