test_that("null template returns null", {
  # setup
  x <- 1

  # execution
  res_empty <- null()
  res_x <- null(x)

  # expectations
  expect_null(res_empty)
  expect_null(res_x)
})

#' List Countries with Missing Indicator Values
#'
#' This function takes a dataframe of WHO indicators and returns a named list.
#' Each element of the list corresponds to an indicator and contains the names
#' of countries (GEO_NAME_SHORT) where the indicator value is NA.
#'
#' @param who_indicators A dataframe with GEO_NAME_SHORT and indicator columns.
#' @return A named list of vectors, each listing countries with NA for that indicator.
#' @export
#'
#' @examples
#' na_list <- list_na_countries(who_indicators)

list_na_countries <- function(who_indicators) {
  indicator_cols <- setdiff(names(who_indicators), "GEO_NAME_SHORT")
  na_list <- lapply(indicator_cols, function(col) {
    who_indicators |>
      dplyr::filter(is.na(.data[[col]])) |>
      dplyr::pull(GEO_NAME_SHORT)
  })
  names(na_list) <- indicator_cols
  return(na_list)
}
