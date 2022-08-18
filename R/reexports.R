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

#' Create a complete ggplot appropriate to a particular data type
#'
#' `autoplot2()` uses ggplot2 to draw a particular plot for an object of a
#' particular class in a single command. This defines the S3 generic that
#' other classes and packages can extend.
#'
#' @param object an object, whose class will determine the behaviour of autoplot
#' @param ... other arguments passed to specific methods
#'
#' @keywords internal
#' @export
#' @seealso `autoplot()`
autoplot2 <- function(object, ...) {
  UseMethod("autoplot2")
}

# ggplot2::autolayer

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
