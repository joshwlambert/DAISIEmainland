#' Convert a `stac` (status of colonist) to a string
#'
#' @inheritParams default_params_doc
#'
#' @return the event as a string
#'
#' @seealso use \link{str_to_event} to convent a string to an `event`
#'
#' @examples
#' DAISIEmainland:::stac_to_str(1)
#' DAISIEmainland:::stac_to_str(2)
#' DAISIEmainland:::stac_to_str(3)
#' DAISIEmainland:::stac_to_str(4)
#' DAISIEmainland:::stac_to_str(5)
#' DAISIEmainland:::stac_to_str(6)
#' @keywords internal
#' @author Richèl J.C. Bilderbeek
stac_to_str <- function(stac) {
  testthat::expect_equal(length(stac), 1)
  if (stac == 1) {
    return("Non_endemic_MaxAge")
  } else if (stac == 2) {
    return("Endemic")
  } else if (stac == 3) {
    return("Endemic&Non_Endemic")
  } else if (stac == 4) {
    return("Non_endemic")
  } else if (stac == 5) {
    return("Endemic_singleton_MaxAge")
  } else if (stac == 6) {
    return("Endemic_clade_MaxAge")
  }
  stop(
    "Invalid event with value ", stac, " \n",
    "Tip: values can be 1, 2, 3, 4, 5 or 6"
  )
}

