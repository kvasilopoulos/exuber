#' Tidy an cv object
#'
#' @param x An `cv` object
#' @inheritParams tidy.radf
#'
#' @return A A [tibble::tibble()]
#'
#' \itemize{
#' \item sig The significance level.
#' \item name The name of the series (when format is "long")
#' \item crit The critical value (when format is "long")
#' }
#'
#'
#' @importFrom purrr keep reduce
#' @importFrom rlang set_names
#' @importFrom dplyr full_join mutate
#' @importFrom tibble rownames_to_column enframe
#'
#' @export
#' @examples
#' \dontrun{
#' mc <- mc_cv(100)
#'
#' # Get the critical values
#' tidy(mc)
#'
#' # Get the critical value sequences
#' augment(mc)
#'
#' }
tidy.mc_cv <- function(x, format = c("wide", "long"), ...) {

  format <- match.arg(format)

    tbl_cv <- x %>%
      keep(names(.) %in% c("adf_cv", "sadf_cv", "gsadf_cv")) %>%
      map(enframe) %>%
      reduce(full_join, by = "name") %>%
      set_names(c("sig", "adf", "sadf", "gsadf")) %>%
      mutate(sig = sub("%$", "", sig) %>% as.factor())

    if (format == "long") {
      tbl_cv <- tbl_cv %>%
        gather(name, crit, -sig) %>%
        select(name, sig, crit)
    }

    tbl_cv
}

#' @rdname tidy.mc_cv
#' @export
tidy.wb_cv <- function(x, format = c("wide", "long"), ...) {

  format <- match.arg(format)

    tbl_cv <- x %>%
      keep(names(.) %in% c("adf_cv", "sadf_cv", "gsadf_cv")) %>%
      map(~ .x %>%
            as.data.frame() %>%
            tibble::rownames_to_column() %>%
            as_tibble() %>%
            gather(name, value, -rowname)) %>%
      reduce(full_join, by = c("rowname", "name")) %>%
      set_names(c("id", "sig", "adf", "sadf", "gsadf")) %>%
      mutate(sig = gsub("%", "", sig) %>% as.factor())

    if (format == "long") {
      tbl_cv <- tbl_cv %>%
        gather(name, crit, -id, -sig) %>%
        select(id, name, sig, crit)
    }

    tbl_cv
}

#' @rdname tidy.mc_cv
#' @export
tidy.sb_cv <- function(x, format = c("wide", "long"), ...) {

  format <- match.arg(format)

    tbl_cv <- x %>%
      pluck("gsadf_panel_cv") %>%
      enframe(name = "sig", value = "gsadf_panel") %>%
      mutate(sig = sub("%$", "", sig) %>% as.factor()) %>%
      select(sig, gsadf_panel)

    if (format == "long") {

      tbl_cv <- tbl_cv %>%
        mutate(id = "panel") %>%
        gather(name, crit, -id, -sig) %>%
        select(id, name, sig, crit)
    }

  tbl_cv
}

#' @rdname tidy.mc_cv
#' @inheritParams tidy.radf
#'
#' @importFrom rlang as_double set_names
#' @importFrom tidyr gather
#' @importFrom dplyr as_tibble bind_cols mutate select bind_rows
#' @importFrom purrr pluck map2 reduce
#' @export
augment.mc_cv <- function(x, format = c("wide", "long"), ...) {

  format <- match.arg(format)

    tbl_cv <- bind_cols(
      x %>%
        pluck("badf_cv") %>%
        as_tibble() %>%
        add_key(x) %>%
        gather(sig, badf, -key),
      x %>%
        pluck("bsadf_cv") %>%
        as_tibble() %>%
        gather(sig, bsadf) %>%
        select(-sig)) %>%
      mutate(sig = sub("%$", "", sig) %>% as.factor())

    if (format == "long") {
      tbl_cv <- tbl_cv %>%
        gather(name, crit, -sig, -key) %>%
        select(key, name, sig, crit)
    }
    tbl_cv
}

#' @rdname tidy.mc_cv
#' @export
augment.wb_cv <- function(x, format = c("wide", "long"), ...) {

  format <- match.arg(format)

    iternames <- x %>% pluck("badf_cv") %>% dimnames() %>% `[[`(3)

    tbl_cv <-
      bind_rows(
      x %>%
        array_to_list("badf_cv") %>%
        set_names(iternames) %>%
        map(as_tibble) %>%
        map2(iternames, ~ .x %>% gather(sig, !!(.y))) %>%
        reduce(bind_cols) %>%
        mutate(name = "badf") %>%
        select(sig, name, iternames),
      x %>%
        array_to_list("bsadf_cv") %>%
        set_names(iternames) %>%
        map(as_tibble) %>%
        map2(iternames, ~ .x %>% gather(sig, !!(.y))) %>%
        reduce(bind_cols) %>%
        mutate(name = "bsadf") %>%
        select(sig, name, iternames)
    ) %>%
      mutate(index = rep(index(x, trunc = TRUE), 6)) %>% # n_sig * n_name
      mutate(key = rep((get_minw(x) + 1):(nrow(.)/6 + get_minw(x)),6)) %>%
      mutate(sig = gsub("%", "", sig) %>% as.factor()) %>%
      select(key, index, sig, name , everything())


    if (format == "long") {
      tbl_cv <- tbl_cv %>%
        gather(id, crit, -key, -index, -sig, -name)
    }
    tbl_cv
}

#' @rdname tidy.mc_cv
#' @export
augment.sb_cv <- function(x, format = c("wide", "long"), ...) {

  format <- match.arg(format)

    tbl_cv <- x %>%
      pluck("bsadf_panel_cv") %>%
      as_tibble() %>%
      add_column(index = index(x, trunc = TRUE)) %>%
      add_key(x) %>%
      gather(sig, bsadf_panel, -index, -key) %>%
      mutate(sig = sub("%$", "", sig) %>% as.factor()) %>%
      select(key, index, everything())

    if (format == "long") {
      tbl_cv <- tbl_cv %>%
        rename(panel = bsadf_panel) %>%
        gather(name, crit, -sig, -index, -key) %>%
        select(key, index, sig, name, crit)
    }
  tbl_cv
}


# mc_distr -----------------------------------------------------------------

#' Tidying *_dist objects
#'
#' tidy `*_dist` takes an `mc_distr`, `wb_distr` or `sb_distr` object and returns
#' a tibble.
#'
#' @param x An `*_dist` object
#' @param ... Additional arguments. Not used.
#'
#' @return A [tibble::tibble()]
#'
#' @importFrom dplyr tibble
#' @export
#' @examples
#' \dontrun{
#' mc <- mc_cv(n = 100)
#'
#' tidy(mc)
#' }
tidy.mc_distr <- function(x, ...) {
  tibble(
    adf = x$adf_cv,
    sadf = x$sadf_cv,
    gsadf = x$gsadf_cv
  )
}

#' Plotting `distr` object
#'
#' Takes `distr`objects and returns a ggplot2 object
#'
#' @param object An `*_dist` object.
#' @param ... Additional arguments, used only in `wb_distr` facet options.
#'
#' @importFrom tidyr gather
#' @importFrom ggplot2 geom_density aes
#' @export
autoplot.mc_distr <- function(object, ...) {

  object %>%
    tidy() %>%
    rename(ADF = adf, SADF = sadf, GSADF = gsadf) %>%
    tidyr::gather(Distribution, value, factor_key = TRUE) %>%
    ggplot(aes(value, fill = Distribution)) +
    geom_density(alpha = 0.2) +
    theme_bw() +
    labs(x = "", y = "",
         title = "Distributions of the ADF and supADF statistics")
}


# wb_distr -----------------------------------------------------------------




#' @rdname tidy.mc_distr
#'
#' @importFrom tidyr gather
#' @importFrom dplyr select bind_cols as_tibble
#' @importFrom purrr reduce pluck
#'
#' @export
tidy.wb_distr <- function(x, ...) {
  list(
    x %>%
      pluck("adf_cv") %>%
      as_tibble() %>%
      tidyr::gather(id, adf),
    x %>%
      pluck("sadf_cv") %>%
      as_tibble() %>%
      gather(id, sadf) %>%
      select(-id),
    x %>%
      pluck("gsadf_cv") %>%
      as_tibble() %>%
      gather(id, gsadf) %>%
      select(-id)
  ) %>%
    reduce(bind_cols)
}

#' @rdname autoplot.mc_distr
#'
#' @importFrom tidyr gather
#' @importFrom ggplot2 facet_wrap
#' @export
autoplot.wb_distr <- function(object, ...) {

  object %>%
    tidy() %>%
    rename(ADF = adf, SADF = sadf, GSADF = gsadf) %>%
    tidyr::gather(Distribution, value, -id, factor_key = TRUE) %>%
    ggplot(aes(value, fill = Distribution)) +
    geom_density(alpha = 0.2) +
    theme_bw() +
    theme(strip.background = element_blank()) +
    facet_wrap(~ id, scales = "free", ...) +
    labs(x = "", y = "")
}

# sb_distr -----------------------------------------------------------------

#' @rdname tidy.mc_distr
#' @export
tidy.sb_distr <- function(x, ...) {
  tibble(
    gsadf_panel = x
  )
}

#' @rdname autoplot.mc_distr
#' @export
autoplot.sb_distr <- function(object, ...) {
  object %>%
    tidy() %>%
    ggplot(aes(gsadf_panel, fill = gsadf_panel)) +
    scale_x_continuous() +
    geom_density(fill = "lightblue") +
    theme_bw() +
    labs(x = "", y = "", title = "Distribution of the Panel supADF statistic")
}
