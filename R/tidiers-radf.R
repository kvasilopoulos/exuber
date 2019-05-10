#' @importFrom purrr keep map reduce
#' @importFrom dplyr full_join arrange
#' @importFrom rlang set_names
#' @importFrom tidyr gather
#' @export
tidy.radf <- function(x, format = c("wide", "long")) {

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

#' @importFrom dplyr rename as_tibble
#' @importFrom tidyr gather
#' @export
augment.radf <- function(x, format = c("wide", "long")) {

  format <- match.arg(format)

  tbl_radf <- x %>%
    pluck("badf") %>%
    as_tibble() %>%
    mutate(
      bsadf_panel = pluck(x, "bsadf_panel"),
      index = index(x, trunc = TRUE)
    ) %>%
    gather(id, badf, -bsadf_panel, -index) %>%
    bind_cols(
      x %>%
        pluck("bsadf") %>%
        as_tibble() %>%
        gather(name, bsadf) %>%
        select(bsadf)
    ) %>%
    select(index, id, everything())

  if (format == "long") {
    tbl_radf <-
      tbl_radf %>%
      gather(name, tstat, -index, -id) %>%
      arrange(id, name)
  }

  tbl_radf

}

#' @importFrom purrr pluck
#' @importFrom rlang set_names
#' @importFrom dplyr full_join as_tibble
#' @export
glance.radf <- function(x, format = c("wide", "long")) {

  format <- match.arg(format)

  tbl_radf <- x %>%
    pluck("gsadf_panel") %>%
    as_tibble() %>%
    set_names("panel")

  if (format == "long") {
    tbl_radf <-
      tbl_radf %>%
      gather(name, tstat)
  }

  tbl_radf

}
