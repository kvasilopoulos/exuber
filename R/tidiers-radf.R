#' Tidy a `radf_obj` object
#'
#' Summarizes information about `radf_obj` object.
#'
#' @param x An object of class `radf_obj`.
#' @param format Long or wide format (default = "wide").
#' @param panel If TRUE then returns the panel statistics
#' @param ... Further arguments passed to methods. Not used.
#'
#' @importFrom purrr keep map reduce pluck
#' @importFrom dplyr full_join arrange
#' @importFrom rlang set_names
#' @importFrom tidyr gather
#' @importFrom tibble enframe
#'
#' @return A [tibble::tibble()]
#'
#' @export
#' @examples
#' \donttest{
#' dta <- data.frame(psy1 = sim_psy1(n = 100), psy2 = sim_psy2(n = 100))
#'
#' rfd <- radf(dta)
#'
#' # Get the test statistic
#' tidy(rfd)
#'
#' # Get the test statisticsequences
#' augment(rfd)
#'
#' # Get the panel test statistic
#' tidy(rfd, panel = TRUE)
#' }
tidy.radf_obj <- function(x, format = c("wide", "long"), panel = FALSE, ...) {

  format <- match.arg(format)

  if (panel) {
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
  }else{
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
  }
  tbl_radf
}

#' @rdname tidy.radf_obj
#'
#' @importFrom dplyr rename as_tibble everything
#' @importFrom tidyr gather pivot_longer drop_na
#' @export
augment.radf_obj <- function(x, format = c("wide", "long"), panel = FALSE, trunc = TRUE, ...) {

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
    tbl_radf <- list(
      extract_mat(x),
      extract_stat(x, "badf"),
      extract_stat(x, "bsadf")
    ) %>%
      reduce(full_join, by = c("key", "id"))

    if(trunc) {
      tbl_radf <- drop_na(tbl_radf, bsadf)
    }

    if (format == "long") {
      tbl_radf <-
        tbl_radf %>%
        pivot_longer(cols = c(badf, bsadf), values_to = "tstat") %>%
        mutate(name = factor(name, levels = c("badf", "bsadf"))) %>%
        arrange(id, name)
    }
  }
  tbl_radf
}

extract_stat <- function(x, stat) {
  pluck(x, stat) %>%
    as_tibble() %>%
    add_key(x) %>%
    pivot_longer(-key, names_to = "id", values_to = stat) %>%
    select(key, everything())
}

extract_mat <- function(x) {
  mat(x) %>%
    as_tibble %>%
    add_key(x, F) %>%
    mutate(index = index(x, trunc = FALSE)) %>%
    pivot_longer(cols = -c(key, index), names_to = "id", values_to = "data") %>%
    select(key, index, everything())
}


