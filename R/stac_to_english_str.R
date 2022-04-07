#' Convert a `stac` (status of colonist) to an English/human-readable string
#'
#' Convert a `stac` (status of colonist) to an English/human-readable string:
#'
#' Value|String
#' -----|------
#' 0    | `[no colonization has taken place]`
#' 1    | `Non-endemic at maximum age`
#' 2    | `Endemic`
#' 3    | `Endemic and non-endemic`
#' 4    | `Non-endemic`
#' 5    | `Endemic singleton at maximum age`
#' 6    | `Endemic clade at maximum age`
#'
#' @inheritParams default_params_doc
#'
#' @return the status of a colonist as a string
#'
#' @seealso use \link{stac_to_str} to convent a `stac` to a single-word
#' computery string
#'
#' @examples
#' DAISIEmainland:::stac_to_english_str(0)
#' DAISIEmainland:::stac_to_english_str(1)
#' DAISIEmainland:::stac_to_english_str(2)
#' DAISIEmainland:::stac_to_english_str(3)
#' DAISIEmainland:::stac_to_english_str(4)
#' DAISIEmainland:::stac_to_english_str(5)
#' DAISIEmainland:::stac_to_english_str(6)
#' @keywords internal
#' @author Richèl J.C. Bilderbeek
stac_to_english_str <- function(stac) {
  DAISIEmainland::check_stac(stac)
  if (stac == 0) {
    return("[no colonization has taken place]")
  } else if (stac == 1) {
    return("Non-endemic at maximum age")
  } else if (stac == 2) {
    return("Endemic")
  } else if (stac == 3) {
    return("Endemic and non-endemic")
  } else if (stac == 4) {
    return("Non-endemic")
  } else if (stac == 5) {
    return("Endemic singleton at maximum age")
  }
  testthat::expect_true(stac == 6)
  "Endemic clade at maximum age"
}
