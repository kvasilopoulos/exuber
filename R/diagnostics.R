#' Diagnostics
#'
#' Finds the series that reject the null hypothesis.
#'
#' @inheritParams summary.radf
#' @param ... further arguments passed to methods.
#'
#' @return Returns a list with the series that reject (positive) and the series
#' that do not reject (negative) the null hypothesis, and at what significance level.
#'
#' @details
#' Diagnostics also stores a vector in {0,1} that corresponds to {postive, negative} respectively.
#'
#' @export
diagnostics <- function(object, cv = NULL, ...) {
  UseMethod("diagnostics")
}

diagnostics.default <- function(object, cv = NULL, ...) {
  stop_glue(
    "method 'diagnostics' is not available for objects of class '{class(object)}'."
  )
}

#' @rdname diagnostics
#' @importFrom dplyr case_when
#' @param option Whether to apply the "gsadf" or "sadf" methodology. Default is "gsadf".
#' @export
diagnostics.radf <- function(object, cv = NULL,
                             option = c("gsadf", "sadf"), ...) {

  assert_class(object, "radf")
  cv <- cv %||% retrieve_crit(object)
  assert_class(cv, "cv")
  assert_match(object, cv)
  option <- match.arg(option)

  if (option == "sadf" && is_sb(cv)) {
    stop_glue("'sadf' does not apply for sieve bootstrapped critical values")
  }
  snames <- series_names(object)
  if (is_sb(cv)) {
    out <- glance_join(object, cv) %>%
      pivot_wider(names_from = sig, values_from = crit, names_prefix = "cv")
  } else {
    out <- tidy_join(object, cv) %>%
      pivot_wider(names_from = sig, values_from = crit, names_prefix = "cv") %>%
      filter(name  == option)
  }

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

  if (is_sb(cv)) {
    positive <- ifelse(length(dummy), "panel", NA)
    negative <- ifelse(length(dummy), NA, "panel")
  } else {
    positive <- snames[as.logical(dummy)]
    negative <- snames[!as.logical(dummy)]
  }

  structure(
    list(
      positive = positive,
      negative = negative,
      sig = sig,
      dummy = dummy
    ),
    panel = is_sb(cv),
    series_names = if (!is_sb(cv)) snames,
    method = get_method(cv),
    option = option,
    class = "diagnostics"
  )
}

#' @export
tidy.diagnostics <- function(x, ...) {
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
    stop_glue("Cannot reject H0 for significance level of 5%")
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
print.diagnostics <- function(x, ...) {

  cli::cat_line()
  cli::cat_rule(
    left = glue('Diagnostics (option = {attr(x, "option")})'),
    right = get_method(x)
  )
  cli::cat_line()
  if (attr(x, "panel")) {
    if (x$sig == "Reject")
      cat(" Cannot rejeact H0 \n")
    else
      cat(" Rejects H0 at the", cli::col_red(x$sig), "significance level.\n")
  } else {
    width <- nchar(series_names(x))
    ngaps <- max(8, width) - width
    for (i in seq_along(series_names(x))) {
      cat(series_names(x)[i], ":" , rep(" ", ngaps[i]), sep = "")
      if (x$sig[i] == "Reject")
        cat(" Cannot reject H0 \n")
      else
        cat(" Rejects H0 at the", cli::col_red(x$sig[i]), "significance level.\n")
    }
  }
  cli::cat_line()
}

