#' Stored Monte Carlo Critical Values
#'
#' A dataset containing Monte Carlo critical values for up to 600 observations
#' generated using the default minimum window. The critical values have been simulated and
#' stored as data to save computation time for the user. The stored critical values
#' can be obtained with the \code{\link[=radf_mc_cv]{radf_mc_cv()}} function, using
#' nrep = `2000` and the `seed = 123`.
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
#' @source Simulated from exuber package function \code{\link[=radf_mc_cv]{radf_mc_cv()}}.
#'
#' @examples
#' \dontrun{
#' all.equal(radf_crit[[50]], radf_mc_cv(50, nrep = 2000, seed = 123))
#' }
"radf_crit"


#' @importFrom tibble enframe
#' @export
print.crit <- function(x, ...) {
  # we dont want to overwhelm the console
  print(x[1:10], ...)
  cat("[ truncated list ]")
}

#' Simulated dataset
#'
#' An artificial dataset containing series simulated from data generating processes
#' widely used in the literature on speculative bubbles.
#'
#' @seealso sim_psy1 sim_psy1 sim_evans sim_div sim_blan
#'
#' @examples
#' \dontrun{
#' # The dataset can be easily replicated with the code below
#' library(tibble)
#' set.seed(1122)
#' sim_data <- tibble(
#'   sim_psy1 = sim_psy1(100),
#'   sim_psy2 = sim_psy2(100),
#'   sim_evans = sim_evans(100),
#'   sim_div = sim_div(100),
#'   sim_blan = sim_blan(100)
#' )
#' sim_data_wdate <- tibble(
#'   psy1 = sim_psy1(100),
#'   psy2 = sim_psy2(100),
#'   evans = sim_evans(100),
#'   div = sim_div(100),
#'   blan = sim_blan(100),
#'   date = seq(as.Date("2000-01-01"), by = "month", length.out = 100)
#' )
#'}
"sim_data"

#' @rdname sim_data
"sim_data_wdate"
