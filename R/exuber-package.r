#' Econometric Analysis of Explosive Time Series
#'
#' Testing for and dating periods of explosive dynamics (exuberance) in time series
#' using the univariate and panel recursive unit root tests proposed by Phillips et al. (2015)
#' <doi:10.1111/iere.12132> and Pavlidis et al. (2016)  <doi:10.1007/s11146-015-9531-2>.
#' The recursive least-squares algorithm utilizes the matrix inversion lemma to avoid matrix
#' inversion which results in significant speed improvements. Simulation of a variety of
#' periodically-collapsing bubble processes.
#'
#' @name exuber
#' @useDynLib exuber, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom glue glue
#' @docType package
#' @keywords internal
"_PACKAGE"
