#' Simulated dataset
#'
#' An artificial dataset containing series simulated from data generating processes
#' widely used in the literature on speculative bubbles.
#'
#' @seealso sim_psy1 sim_psy1 sim_evans sim_div sim_blan
#'
#' @keywords internal
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
#' }
#'
#' @examples
#' # Explore the five bundled series before running any test
#' matplot(sim_data, type = "l", lty = 1, xlab = "t", ylab = "value",
#'   main = "sim_data: the five bundled example series")
#' legend("topleft", legend = colnames(sim_data), col = 1:5, lty = 1, bty = "n")
#'
#' \donttest{
#' # The usual next step: run radf() and plot with the bundled critical values
#' autoplot(radf(sim_data))
#' }
"sim_data"

#' @rdname sim_data
"sim_data_wdate"
