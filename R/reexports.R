#' @importFrom zoo index
#' @seealso \code{\link[=index.radf]{index.radf()}}
#' @export
zoo::index

#' @importFrom zoo index<-
#' @export
zoo::`index<-`

#' @importFrom ggplot2 fortify
#' @seealso \code{\link[=fortify.radf]{fortify.radf()}}
#' \code{\link[=fortify.datestamp]{fortify.datestamp()}}
#' @export
ggplot2::fortify

#' @importFrom ggplot2 autoplot
#' @seealso \code{\link[=autoplot.radf]{autoplot.radf()}}
#' \code{\link[=autoplot.datestamp]{autoplot.datestamp()}}
#' @export
ggplot2::autoplot


#' @importFrom generics tidy
#' @seealso \code{\link[=tidy.radf]{tidy.radf()}}
#'  \code{\link[=tidy.mc_cv]{tidy.mc_cv()}}
#'  \code{\link[=tidy.wb_cv]{tidy.wb_cv()}}
#'  \code{\link[=tidy.sb_cv]{tidy.sb_cv()}}
#'  \code{\link[=tidy.mc_distr]{tidy.mc_distr()}}
#'  \code{\link[=tidy.wb_distr]{tidy.wb_distr()}}
#'  \code{\link[=tidy.sb_distr]{tidy.sb_distr()}}
#' @export
generics::tidy

#' @importFrom generics augment
#' @seealso \code{\link[=augment.radf]{augment.radf()}}
#'  \code{\link[=augment.mc_cv]{augment.mc_cv()}}
#'  \code{\link[=augment.wb_cv]{augment.wb_cv()}}
#'  \code{\link[=augment.sb_cv]{augment.sb_cv()}}
#' @export
generics::augment

#' @importFrom generics glance
#' @seealso \code{\link[=glance.radf]{glance.radf()}}
#' @export
generics::glance

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
NULL


