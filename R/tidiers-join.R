#' Tidy into a joint model
#'
#' Tidy a model of `radf` with a model of `cv`
#'
#' @param x An object of classs `radf`
#' @param y An object of classs `cv`
#'
#' @export
augment_join <- function(x, y) {

  if (!inherits(x, "radf"))
    stop("`x` should be of class `radf`", call. = FALSE)

  if (!inherits(y, "cv"))
    stop("`y` should be of class `cv`", call. = FALSE)

  if (method(y) == "Sieve Bootstrap") {
    tbl <- right_join(
      augment(radf_dta, "long", panel = TRUE),
      augment(sb, "long"),
      by = c("key", "name")
    )
  }else{
    tbl <- right_join(
      augment(x, "long"),
      augment(y, "long"),
      by = c("key", "name")
    )
  }

  tbl %>%
    select(-key)

}
