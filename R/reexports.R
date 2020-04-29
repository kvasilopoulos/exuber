#' @importFrom ggplot2 fortify
#' @seealso \code{\link[=fortify.radf_obj]{fortify.radf_obj()}}
#' \code{\link[=fortify.ds_radf]{fortify.ds_radf()}}
#' @export
ggplot2::fortify

#' @importFrom ggplot2 autoplot
#' @seealso \code{\link[=autoplot.radf_obj]{autoplot.radf_obj()}}
#' \code{\link[=autoplot.ds_radf]{autoplot.ds_radf()}}
#' @export
ggplot2::autoplot

#' @importFrom generics tidy
#' @seealso \code{\link[=tidy.radf_obj]{tidy.radf_obj()}}
#'  \code{\link[=tidy.radf_cv]{tidy.radf_cv()}}
#'  \code{\link[=tidy.radf_distr]{tidy.radf_distr()}}
#' @export
generics::tidy

#' @importFrom generics augment
#' @seealso \code{\link[=augment.radf_obj]{augment.radf_obj()}}
#'  \code{\link[=augment.radf_cv]{augment.radf_cv()}}
#' @export
generics::augment

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
NULL
