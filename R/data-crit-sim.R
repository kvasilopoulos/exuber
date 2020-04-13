#' Simulated Monte Carlo critical values
#'
#' A dataset containing simulated critical values for up to 600 observations
#' based on default minimum window. The critical values have been simulated and
#' stored as data to save computation time for the user. The stored critical values
#' can be obtained with the \code{\link[=mc_cv]{mc_cv()}} function, using the `seed = 123`.
#'
#'
#' @format A list with lower level lists that contain
#' \describe{
#'   \item{adf_cv:}{Augmented Dickey-Fuller}
#'   \item{badf_cv:}{Backward Augmented Dickey-Fuller}
#'   \item{sadf_cv:}{Supremum Augmented Dickey-Fuller}
#'   \item{bsadf_cv:}{Backward Supremum Augmented Dickey-Fuller}
#'   \item{gsadf_cv:}{Generalized Supremum Augmented Dickey Fuller}
#'
#' }
#' @source simulated from exuber package function \code{\link[=mc_cv]{mc_cv()}}
#'
#' @examples
#' \dontrun{
#' all.equal(crit[[50]], mc_cv(50, seed = 123))
#' }
"crit"


#' @importFrom tibble enframe
print.crit <- function(x, ...) {
  # we dont want to overwhelm the console
  print(tibble::enframe(x), ...)
}

#' Simulated dataset
#'
#' A dataset containing commonly known data generated processes (DGPs) used in the explosive
#' time series literature.
#'
#' @seealso sim_psy1 sim_psy1 sim_evans sim_div sim_blan
#'
#' @examples
#' # The dataset can be easily replicated with the code belows
#' set.seed(1234)
#' sim_data <- tibble(
#'   sim_psy1 = sim_psy1(100),
#'   sim_psy2 = sim_psy2(100),
#'   sim_evans = sim_evans(100),
#'   sim_div = sim_div(100),
#'   sim_blan = sim_blan(100)
#' )
#'
"sim_data"
