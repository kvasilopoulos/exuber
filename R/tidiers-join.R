#' Tidy into a joint model
#'
#' Tidy, agument or glance, and then join objects of class `radf` and `cv`. The
#' object of reference should be the `radf`. For example, using `glance` in an
#' radf object returns the panel statistic, so `glance_join` returns the panel
#' statistic together with the critical values.
#'
#' @param x An object of class `radf`.
#' @param y An object of class `cv`. The output will depend on the type of
#' critical value.
#'
#'
#' @importFrom dplyr full_join case_when
#' @export
tidy_join <- function(x, y = NULL) {

  assert_class(x, "radf")
  y <- y %||% retrieve_crit(x)
  assert_class(y, "cv")
  if (is_sb(y)) {
    stop_glue(
      "argument 'y' should not be of class 'sb_cv', ",
      "do you need 'glance_join'")
  }
  assert_match(x, y)

  join_by <- if (!is_mc(y)) c("id") else NULL
  inner_join(
    tidy(x, format = "long"),
    tidy(y, format = "long"),
    by = c("name", join_by)) %>%
    arrange(name)
}


#' @rdname tidy_join
#' @importFrom dplyr inner_join select case_when
#' @export
augment_join <- function(x, y = NULL) {

  assert_class(x, "radf")
  y <- y %||% retrieve_crit(x)
  assert_class(y, "cv")
  assert_match(x, y)

  panel_arg <- is_sb(y)
  join_by <- if (!is_mc(y)) c("index", "id") else NULL

  inner_join(
    augment(x, "long", panel = panel_arg),
    augment(y, "long"), by = c("key", "name", join_by)
  ) %>%
    arrange(sig, id, name) %>%
    select(-key)
}

#' @rdname tidy_join
#' @importFrom dplyr inner_join select case_when
#' @export
glance_join <- function(x, y) {
  if (!is_sb(y)) {
    stop_glue("argument 'y' should be of class 'sb_cv'")
  }
  inner_join(
    glance(x, format = "long"),
    tidy(y, format = "long"),
    by = c("id", "name")) %>%
    arrange(name)
}
