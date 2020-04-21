#' Tidy into a joint model
#'
#' Tidy, augment or glance, and then join objects of class `radf` and `cv`. The
#' object of reference should be the `radf`. For example, using `glance` in an
#' radf object returns the panel statistic, so `glance_join` returns the panel
#' statistic together with the critical values.
#'
#' @param x An object of class `radf`.
#' @param y An object of class `cv`. The output will depend on the type of
#' critical value.
#'
#'
#' @importFrom dplyr full_join case_when select_at
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
    mutate(
      id = factor(id, levels = series_names(x)),
      name = factor(name, levels = c("adf", "sadf", "gsadf"))) %>%
    arrange(id,name)
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
  join_by <- if (!is_mc(y)) c("id") else NULL
  is_idx_date <- is.Date(index(x))
  if (!is_idx_date && !is_mc(y)) join_by <- c("index", join_by)
  idx_if_date <- if (is_idx_date && !is_mc(y)) "index"  else NULL
  key_if_date <- if (is_idx_date) "key"  else NULL
  id_lvls <- if (is_sb(y)) "panel" else series_names(x)

  inner_join(
    augment(x, "long", panel = panel_arg),
    augment(y, "long") %>% select_at(vars(-all_of(idx_if_date))),
    by = c("key", "name", join_by)) %>%
    mutate(id = factor(id, levels = id_lvls)) %>%
    arrange(sig, id, name) %>%
    select_at(vars(-all_of(key_if_date)))
}

#' @importFrom vctrs vec_as_location
all_of <- function(x) {
  if (is.function(x)) {
    vctrs::vec_as_location(x, 0L)
  }
  x
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
