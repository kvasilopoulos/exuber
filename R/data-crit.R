#' Simulated Monte Carlo critical values
#'
#' A dataset containing the simulated critical values for 2000 observations
#' with default minimum window. The critical values have been simulated and stored
#' as data to save computationan time for the user. The critical values can be
#' replicated with the help of the \code{\link[=mc_cv]{mc_cv()}} function. Note
#' that the results may differ due to the randomness of the Monte Carlo trials.
#'
#'
#' @format A list with lower level lists that contain
#' \describe{
#'   \item{adf:}{Augmented Dickey-Fuller}
#'   \item{badf:}{Backward Augmented Dickey-Fuller}
#'   \item{sadf:}{Supremum Augmented Dickey-Fuller}
#'   \item{bsadf:}{Backward Supremum Augmented Dickey-Fuller}
#'   \item{gsadf:}{Generalized Supremum Augmented Dickey Fuller}
#'   ...
#' }
#' @source simulated from exuber package function \code{\link[=mc_cv]{mc_cv()}}
"crit"

#'@export
#'@importFrom tibble enframe
print.crit <- function(x, ...) {
  # we dont want to overwhelm the console
  print(tibble::enframe(x))
}

