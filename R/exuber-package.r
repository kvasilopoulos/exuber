#' Econometric Analysis of Explosive Time Series
#'
#' Testing for and dating periods of explosive dynamics (exuberance) in time
#' series using recursive unit root tests as proposed by
#' \href{https://doi.org/10.1111/iere.12132}{Phillips, P. C., Shi,
#' S. and Yu, J. 2015a}. Simulate a variety of periodically-collapsing bubble
#' models. The estimation and simulation utilizes the matrix inversion lemma
#' from the recursive least squares algorithm, which results in a significant
#' speed improvement.
#'
#' @name exuber
#' @useDynLib exuber, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @docType package
#' @keywords internal
"_PACKAGE"
