#' Diagnostics on hypothesis testing
#'
#' Provides information on whether the null hypothesis of a unit root is rejected
#' against the alternative of explosive behaviour for each series in a dataset.
#'
#' @param object  An object of class `obj`.
#' @param cv An object of class `cv`.
#' @param ... Further arguments passed to methods.
#'
#' @return Returns a list with the series that reject (positive) and the series
#' that do not reject (negative) the null hypothesis, and at what significance level.
#'
#' @details
#' Diagnostics also stores a vector whose elements take the value of 1 when
#' there is a period of explosive behaviour and 0 otherwise.
#'
#' @export
diagnostics <- function(object, cv = NULL, ...) {
  UseMethod("diagnostics")
}

#' @rdname diagnostics
#' @importFrom dplyr case_when
#' @param option Whether to apply the "gsadf" or "sadf" methodology (default = "gsadf").
#' @export
#' @examples
#'
#' rsim_data <- radf(sim_data)
#' diagnostics(rsim_data)
#'
#' diagnostics(rsim_data, option = "sadf")
diagnostics.radf_obj <- function(object, cv = NULL,
                             option = c("gsadf", "sadf"), ...) {

  # assert_class(object, "radf")
  cv <- cv %||% retrieve_crit(object)
  assert_class(cv, "radf_cv")
  assert_match(object, cv)
  option <- match.arg(option)

  if (option == "sadf" && is_sb(cv)) {
    stop_glue("argument 'option' cannot  be be set to 'sadf' when cv is of class 'sb_cv'")
  }
  snames <- series_names(object)
  if (is_sb(cv)) {
    option <- "gsadf_panel"
  }
  out <- tidy_join(object, cv) %>%
    pivot_wider(names_from = sig, values_from = crit, names_prefix = "cv") %>%
    filter(name  == option)
  # in case of simulation exercises
  dummy <- case_when(
    out$tstat < out$cv95 ~ 0,
    out$tstat >= out$cv95 ~ 1
  )
  sig <- case_when(
    out$tstat < out$cv90 ~ "Reject",
    out$tstat >= out$cv90 & out$tstat < out$cv95 ~ "10%",
    out$tstat >= out$cv95 & out$tstat < out$cv99 ~ "5%",
    out$tstat >= out$cv99 ~ "1%"
  )
  dummy_lgl <- as.logical(dummy)
  if (is_sb(cv)) {
    positive <- ifelse(dummy_lgl , "panel", NA)
    negative <- ifelse(dummy_lgl, NA, "panel")
  } else {
    positive <- snames[as.logical(dummy_lgl)]
    negative <- snames[!as.logical(dummy_lgl)]
  }
  list(
    positive = positive,
    negative = negative,
    sig = sig,
    dummy = dummy
  ) %>%
    add_attr(
      panel = is_sb(cv),
      series_names = if (!is_sb(cv)) snames,
      method = get_method(cv),
      option = option,
    ) %>%
    add_class(
      "dg_radf", "dg"
    )
}

#' @export
tidy.dg_radf <- function(x, ...) {
  snames <- series_names(x)
  sig <- gsub("%", "", x$sig)
  tibble(
    "series" = snames,
    "positive" = ifelse(snames %in% x$positive, TRUE, FALSE),
    "negative" = ifelse(snames %in% x$negative, TRUE, FALSE),
    "sig" = as.factor(ifelse(sig == "Reject", NA, sig))
  )
}


diagnostics_internal <- function(...) {
  dg <- diagnostics(...)
  if (all(dg$dummy == 0)) {
    stop_glue("Cannot reject H0 at the 5% significance level")
  }
  if (purrr::is_bare_character(dg$positive, n = 0)) {
    stop_glue("Cannot reject H0")
  }
  dg
}


#' @importFrom cli cat_line cat_rule
#' @importFrom glue glue
#' @importFrom rlang is_bare_character
#' @export
print.dg_radf <- function(x, ...) {

  cli::cat_line()
  cli::cat_rule(
    left = glue('Diagnostics (option = {attr(x, "option")})'),
    right = get_method(x)
  )
  cli::cat_line()
  if (attr(x, "panel")) {
    if (x$sig == "Reject")
      cat(" Cannot reject H0 \n")
    else
      cat(" Rejects H0 at the", cli::col_red(x$sig), "significance level\n")
  } else {
    width <- nchar(series_names(x))
    ngaps <- max(8, width) - width
    for (i in seq_along(series_names(x))) {
      cat(series_names(x)[i], ":" , rep(" ", ngaps[i]), sep = "")
      if (x$sig[i] == "Reject")
        cat(" Cannot reject H0 \n")
      else
        cat(" Rejects H0 at the", cli::col_red(x$sig[i]), "significance level\n")
    }
  }
  cli::cat_line()
}

