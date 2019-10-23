#' Tidy into a joint model
#'
#' Tidy a model of `radf` with a model of `cv`
#'
#' @param x An object of class `radf`
#' @param y An object of class `cv`
#'
#' @importFrom dplyr inner_join select
#' @export
augment_join <- function(x, y = NULL) {

  assert_class(x, "radf")
  if (is.null(y)) {
    y <- retrieve_crit(x)
  }
  assert_class(y, "cv")
  assert_match(x, y)
  if (is_sb(y)) {
    tbl <- inner_join(
      augment(x, "long", panel = TRUE),
      augment(y, "long"),
      by = c("key", "name", "index")
    )
  }else if (is_wb(y)) {
    tbl <- inner_join(
      augment(x, "long"),
      augment(y, "long"),
      by = c("key","index", "name", "id")
    ) %>%
      arrange(id, name, sig)
  }else if (is_mc(y)) {
    tbl <- inner_join(
      augment(x, "long"),
      augment(y, "long"),
      by = c("key", "name")
    )
  }

  tbl %>%
    select(-key)

}
