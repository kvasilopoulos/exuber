#' Tidy an radf object
#'
#' @param x An `radf` object
#' @param format Long or wide format
#' @param ... Additional arguments. Not used.
#'
#' @importFrom purrr keep map reduce
#' @importFrom dplyr full_join arrange
#' @importFrom rlang set_names
#' @importFrom tidyr gather
#'
#' @return A [tibble::tibble()]
#'
#' @export
#' @examples
#' \dontrun{
#' dta <- data.frame(psy1 = sim_psy1(n = 100), psy2 = sim_psy2(n = 100))
#'
#' rfd <- radf(data)
#'
#' # Get the t-stat
#' tidy(rfd)
#'
#' # Get the t-stat sequences
#' augment(rfd)
#'
#' # Get the panel t-stat
#' glance(mc)
#' }
tidy.radf <- function(x, format = c("wide", "long"), ...) {

  format <- match.arg(format)

  tbl_radf <-
    x %>%
    keep(names(.) %in% c("adf", "sadf", "gsadf")) %>%
    map(enframe) %>%
    reduce(full_join, by = "name") %>%
    set_names(c("id", "adf", "sadf", "gsadf"))

  if (format == "long") {
    tbl_radf <-
      tbl_radf %>%
      gather(name, tstat, -id) %>%
      arrange(id)
  }

  tbl_radf
}

#' @rdname tidy.radf
#' @param panel Either univariate or panel bsadf.
#'
#' @importFrom dplyr rename as_tibble everything
#' @importFrom tidyr gather
#' @export
augment.radf <- function(x, format = c("wide", "long"), panel = FALSE, ...) {

  format <- match.arg(format)
  stopifnot(is.logical(panel))

  if (panel) {
    tbl_radf <- tibble(
      index = index(x, trunc = TRUE),
      "panel" = pluck(x, "bsadf_panel")
      ) %>%
      add_key(x) %>%
      select(key, index, panel)

    if (format == "long") {
      tbl_radf <-
        tbl_radf %>%
        gather(name, tstat, -index, -key, factor_key = TRUE)
    }
  }else{
    tbl_radf <- x %>%
      pluck("badf") %>%
      as_tibble() %>%
      add_key(x) %>%
      mutate(
        index = index(x, trunc = TRUE)
      ) %>%
      gather(id, badf, -index, -key, factor_key = TRUE) %>%
      bind_cols(
        x %>%
          pluck("bsadf") %>%
          as_tibble() %>%
          gather(name, bsadf) %>%
          select(bsadf)
      ) %>%
      select(key, index, id, everything())

    if (format == "long") {
      tbl_radf <-
        tbl_radf %>%
        gather(name, tstat, -index, -id, -key) %>%
        arrange(id, name)
    }
  }
  tbl_radf
}

#' @rdname tidy.radf
#'
#' @importFrom purrr pluck
#' @importFrom rlang set_names
#' @importFrom dplyr full_join
#' @importFrom tibble enframe
#' @export
glance.radf <- function(x, format = c("wide", "long"), ...) {

  format <- match.arg(format)
  tbl_radf <- x %>%
    pluck("gsadf_panel") %>%
    enframe(name = NULL) %>%
    set_names("panel")

  if (format == "long") {
    tbl_radf <-
      tbl_radf %>%
      gather(id, tstat)
  }
  tbl_radf
}






