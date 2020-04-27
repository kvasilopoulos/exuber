#' Tidy `radf_obj` objects
#'
#' Summarizes information of `radf_obj` objects.
#'
#' @param x An object of class `radf_obj`.
#' @param format Long or wide format
#' @param ... Further arguments passed to methods. Not used.
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
#' \dottest{
#' dta <- data.frame(psy1 = sim_psy1(n = 100), psy2 = sim_psy2(n = 100))
#'
#' rfd <- radf(dta)
#'
#' # Get the t-stat
#' tidy(rfd)
#'
#' # Get the t-stat sequences
#' augment(rfd)
#'
#' # Get the panel t-stat
#' glance(rfd)
#' }
tidy.radf_obj <- function(x, format = c("wide", "long"), ...) {

  format <- match.arg(format)

  tbl_radf <-
    x %>%
    keep(names(.) %in% c("adf", "sadf", "gsadf")) %>%
    map(enframe) %>%
    reduce(full_join, by = "name") %>%
    set_names(c("id", "adf", "sadf", "gsadf")) %>%
    mutate(id = factor(id, series_names(x)))

  if (format == "long") {
    tbl_radf <-
      tbl_radf %>%
      gather(name, tstat, -id) %>%
      mutate(name = factor(name, levels = c("adf", "sadf", "gsadf"))) %>%
      arrange(id)
  }
  tbl_radf
}

#' @rdname tidy.radf_obj
#' @param panel Either univariate or panel bsadf.
#'
#' @importFrom dplyr rename as_tibble everything
#' @importFrom tidyr gather
#' @export
augment.radf_obj <- function(x, format = c("wide", "long"), panel = FALSE, ...) {

  format <- match.arg(format)
  stopifnot(is.logical(panel))

  if (panel) {
    tbl_radf <- tibble(
      index = index(x, trunc = TRUE),
      bsadf_panel = pluck(x, "bsadf_panel")
      ) %>%
      add_key(x) %>%
      select(key, index, bsadf_panel)

    if (format == "long") {
      tbl_radf <-
        tbl_radf %>%
        gather(name, tstat, -index, -key, factor_key = TRUE) %>%
        mutate(id = factor("panel")) %>%
        select(key, index, id, name, tstat)
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
        mutate(name = factor(name, levels = c("badf", "bsadf"))) %>%
        arrange(id, name)
    }
  }
  tbl_radf
}

#' @rdname tidy.radf_obj
#'
#' @importFrom purrr pluck
#' @importFrom rlang set_names
#' @importFrom dplyr full_join
#' @importFrom tibble enframe
#' @export
glance.radf_obj <- function(x, format = c("wide", "long"), ...) {

  format <- match.arg(format)
  tbl_radf <- x %>%
    pluck("gsadf_panel") %>%
    enframe(name = NULL, value = "gsadf_panel")

  if (format == "long") {
    tbl_radf <-
      tbl_radf %>%
      gather(name, tstat) %>%
      mutate(
        id = factor("panel"),
        name = factor(name)) %>%
      select(id, name, tstat)
  }
  tbl_radf
}


