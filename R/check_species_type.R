#' Check if a `species_type` is valid.
#'
#' Check if a `species_type` is valid.
#' Will \link{stop} if not
#'
#' The type of species:
#'
#'  * `"A"`: the extant clade is of anagenetic origin
#'  * `"C"`: the extant clade is of cladogenetic origin
#'  * `"I"`: the extant clade is of immigrant origin
#'
#' A valid `species_type` ...
#'
#'  * is numeric
#'  * has one element
#'  * the element has a whole-number value in range `[1, 6]`
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @seealso Use \link{species_type_to_str} to convert a `species_type` to a
#' string.
#'
#' @examples
#' check_species_type("A")
#' check_species_type("C")
#' check_species_type("I")
#' @author Richèl J.C. Bilderbeek
#' @export
check_species_type <- function(species_type) {
  testthat::expect_equal(length(species_type), 1)
  testthat::expect_true(is.character(species_type))
  testthat::expect_true(species_type %in% c("A", "C", "I"))

  invisible(species_type)
}
