#' Convert a `colonist_species_type` to a string
#'
#' Convert a `colonist_species_type` to a string:
#'
#' Value|String
#' -----|-----------------------------
#' `A`  | `Colonist after anagenesis`
#' `C`  | `Colonist after cladogenesis`
#'
#' @inheritParams default_params_doc
#'
#' @return the `colonist_species_type` as a string
#'
#' @examples
#' DAISIEmainland:::colonist_species_type_to_str("A")
#' DAISIEmainland:::colonist_species_type_to_str("C")
#' @keywords internal
#' @author Richèl J.C. Bilderbeek
colonist_species_type_to_str <- function(colonist_species_type) { # nolint Indeed a long internal function name
  testthat::expect_equal(length(colonist_species_type), 1)
  if (colonist_species_type == "A") {
    return("Colonist after anagenesis")
  } else if (colonist_species_type == "C") {
    return("Colonist after cladogenesis")
  }
  stop(
    "Invalid 'colonist_species_type' with value ", colonist_species_type, " \n",
    "Tip: values can be 'A' or 'C'"
  )
}
