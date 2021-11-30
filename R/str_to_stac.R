#' Convert a string to a `stac`
#'
#' @param stac_str the `stac` as a string
#'
#' @return the `stac` as a string
#'
#' @seealso use \link{stac_to_str} to convent an `stac` to a string
#'
#' @examples
#' DAISIEmainland:::str_to_stac("Non_endemic_MaxAge")
#' DAISIEmainland:::str_to_stac("Endemic")
#' DAISIEmainland:::str_to_stac("Endemic&Non_Endemic")
#' DAISIEmainland:::str_to_stac("Non_endemic_MaxAge")
#' DAISIEmainland:::str_to_stac("Endemic_singleton_MaxAge")
#' DAISIEmainland:::str_to_stac("Endemic_clade_MaxAge")
#'
#' @keywords internal
#' @author Richèl J.C. Bilderbeek
str_to_stac <- function(stac_str) {
  testthat::expect_equal(length(stac_str), 1)
  if (stac_str == "Non_endemic_MaxAge") {
    return(1)
  } else if (stac_str == "Endemic") {
    return(2)
  } else if (stac_str == "Endemic&Non_Endemic") {
    return(3)
  } else if (stac_str == "Non_endemic") {
    return(4)
  } else if (stac_str == "Endemic_singleton_MaxAge") {
    return(5)
  } else if (stac_str == "Endemic_clade_MaxAge") {
    return(6)
  }
  stop(
    "Invalid 'stac_str' with value ", stac_str, " \n",
    "Tip: values can be 'immigration', 'extinction', 'anagenesis', ",
    "or 'cladogenesis'"
  )
}
