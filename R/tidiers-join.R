#' Tidy into a joint model
#'
#' Tidy and join or augment and join objects of class `radf` and `cv`.
#'
#' @param x An object of class `radf`.
#' @param y An object of class `cv`.
#'
#' @importFrom dplyr full_join case_when
#' @export
tidy_join <- function(x, y = NULL) {

  assert_class(x, "radf")
  y <- y %||% retrieve_crit(x)
  assert_class(y, "cv")
  assert_match(x, y)


  if (is_mc(y)) {
    tbl <- inner_join(
      tidy(x, format = "long"),
      tidy(y, format = "long"),
      by = "name")
  }else if (is_wb(y)) {
    tbl <- inner_join(
      tidy(x, format = "long"),
      tidy(y, format = "long"),
      by = c("name", "id"))
  }else if (is_sb(y)) {
    tbl <- inner_join(
      glance(x, format = "long"),
      tidy(y, format = "long"),
      by = c("id", "name"))
  }
  tbl %>%
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

  #TODO reformat this code to make it smaller

  if (is_mc(y)) {
    tbl <- inner_join(
      augment(x, "long"),
      augment(y, "long"),
      by = c("key", "name")
    )
  } else if (is_wb(y)) {
    tbl <- inner_join(
      augment(x, "long"),
      augment(y, "long"),
      by = c("key","index", "id", "name")
    ) %>%
      arrange(id, name, sig)
  } else if (is_sb(y)) {
    tbl <- inner_join(
      augment(x, "long", panel = TRUE),
      augment(y, "long"),
      by = c("key", "index", "id", "name")
    )
  }
  tbl %>%
    select(-key)
}
