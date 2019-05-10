#' @importFrom purrr keep reduce
#' @importFrom rlang set_names
#' @importFrom dplyr full_join mutate
#' @importFrom tibble rownames_to_column enframe
#' @export
tidy.cv <- function(x, format = c("wide", "long")) {

  format <- match.arg(format)

  if (method(x) == "Monte Carlo") {

    tbl_cv <- x %>%
      keep(names(.) %in% c("adf_cv", "sadf_cv", "gsadf_cv")) %>%
      map(enframe) %>%
      reduce(full_join, by = "name") %>%
      set_names(c("sig", "adf", "sadf", "gsadf")) %>%
      mutate(sig = sub("%$", "", sig))

    if (format == "long") {
      tbl_cv <- tbl_cv %>%
        gather(name, crit, -sig) %>%
        select(name, sig, crit)
    }

  }else if (method(x) == "Wild Bootstrap") {

    tbl_cv <- x %>%
      keep(names(.) %in% c("adf_cv", "sadf_cv", "gsadf_cv")) %>%
      map(~ .x %>%
            as.data.frame() %>%
            tibble::rownames_to_column() %>%
            as_tibble() %>%
            gather(name, value, -rowname)) %>%
      reduce(full_join, by = c("rowname", "name")) %>%
      set_names(c("id", "sig", "adf", "sadf", "gsadf"))

    if (format == "long") {
      tbl_cv <- tbl_cv %>%
        gather(name, crit, -id, -sig) %>%
        select(id, name, sig, crit)
    }

  }else{

    tbl_cv <- x %>%
      pluck("gsadf_panel_cv") %>%
      enframe(name = "sig", value = "gsadf_panel") %>%
      mutate(sig = sub("%$", "", sig)) %>%
      select(sig, gsadf_panel)

    if (format == "long") {

      tbl_cv <- tbl_cv %>%
        mutate(id = "panel") %>%
        gather(name, crit, -id, -sig) %>%
        select(id, name, sig, crit)
    }
  }

  tbl_cv

}


#' @importFrom rlang as_double set_names
#' @importFrom tidyr gather
#' @importFrom dplyr as_tibble bind_cols mutate select
#' @importFrom purrr pluck map2 reduce
#' @export
augment.cv <- function(x, format = c("wide", "long")) {

  format <- match.arg(format)

  if (method(x) == "Monte Carlo") {

    tbl_cv <- bind_cols(
      x %>%
        pluck("badf_cv") %>%
        as_tibble() %>%
        gather(sig, badf),
      x %>%
        pluck("bsadf_cv") %>%
        as_tibble() %>%
        gather(sig, bsadf) %>%
        select(-sig))

    if (format == "long") {
      tbl_cv <- tbl_cv %>%
        gather(name, crit, -sig) %>%
        select(name, sig, crit)
    }
  } else if (method(x) == "Wild Bootstrap") {

    iternames <- x %>% pluck("badf_cv") %>% dimnames() %>% `[[`(3)

    tbl_cv <- bind_rows(
      x %>%
        array_to_list("badf_cv", iternames) %>%
        set_names(iternames) %>%
        map(as_tibble) %>%
        map2(iternames, ~ .x %>% gather(sig, !!(.y))) %>%
        reduce(bind_cols) %>%
        mutate(name = "badf") %>%
        select(sig, name, iternames),
      x %>%
        array_to_list("bsadf_cv", iternames) %>%
        set_names(iternames) %>%
        map(as_tibble) %>%
        map2(iternames, ~ .x %>% gather(sig, !!(.y))) %>%
        reduce(bind_cols) %>%
        mutate(name = "bsadf") %>%
        select(sig, name, iternames)
    )


    if (format == "long") {
      tbl_cv <- tbl_cv %>%
        gather(id, crit, -sig, -name) %>%
        select(id, name, sig, crit)
    }

  } else {

    tbl_cv <- x %>%
      pluck("bsadf_panel_cv") %>%
      as_tibble() %>%
      gather(sig, bsadf_panel) %>%
      mutate(sig = sub("%$", "", sig) %>% as_factor())

    if (format == "long") {
      tbl_cv <- tbl_cv %>%
        gather(name, crit, -sig) %>%
        mutate(id = "panel") %>%
        select(id, name, sig, crit)
    }
  }

  tbl_cv
}















