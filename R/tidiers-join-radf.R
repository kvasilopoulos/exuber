#' Tidy into a joint model
#'
#' Tidy or augment and then join objects.
#'
#' @param x An object of class `obj`.
#' @param y An object of class `cv`.
#' @param ... Further arguments passed to methods.
#' @export
tidy_join <- function(x, y, ...) {
  UseMethod("tidy_join")
}

#' @rdname tidy_join
#' @export
augment_join <- function(x, y, ...) {
  UseMethod("augment_join")
}



#' Tidy into a joint model
#'
#' Tidy or augment  and then join objects of class `radf_obj` and `radf_cv`. The
#' object of reference is the `radf_cv`. For example, if panel critical values
#' are provided the function will return the panel test statistic.
#'
#' @param x An object of class `radf_obj`.
#' @param y An object of class `radf_cv`. The output will depend on the type of
#' critical value.
#' @param  ... Further arguments passed to methods. Not used.
#'
#' @details `tidy_join` also calls `augment_join` when `cv` is of class `sb_cv`.
#'
#' @importFrom dplyr full_join case_when select_at
#' @export
tidy_join.radf_obj <- function(x, y = NULL, ...) {

  y <- y %||% retrieve_crit(x)
  assert_class(y, "radf_cv")
  assert_match(x, y)
  if (is_sb(y)) {
    tbl <- inner_join(
      tidy(x, format = "long", panel = TRUE),
      tidy(y, format = "long"),
      by = c("id", "name")) %>%
      arrange(name)
    return(tbl)
  }

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

#' @export
#' @rdname tidy_join.radf_obj
#' @importFrom dplyr inner_join select case_when
augment_join.radf_obj <- function(x, y = NULL, ...) {

  y <- y %||% retrieve_crit(x)
  assert_class(y, "radf_cv")
  assert_match(x, y)

  is_panel <- is_sb(y)
  join_by <- if (!is_mc(y)) c("id") else NULL
  is_idx_date <- is.Date(index(x))
  if (!is_idx_date && !is_mc(y)) join_by <- c("index", join_by)
  idx_if_date <- if (is_idx_date && !is_mc(y)) "index"  else NULL
  key_if_date <- if (is_idx_date) "key"  else NULL
  id_lvls <- if (is_panel) "panel" else series_names(x)

  inner_join(
    augment(x, "long", panel = is_panel),
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






