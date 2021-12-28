#' Check if a `missing_species` is valid.
#'
#' Check if a `missing_species` is valid.
#' Will \link{stop} if not
#'
#' A valid `missing_species` ...
#'
#'  * is numeric
#'  * has one element
#'  * the element has a whole-number value in range `[0, Inf>`
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @examples
#' check_missing_species(1)
#' @author Richèl J.C. Bilderbeek
#' @export
check_missing_species <- function(missing_species) {
  testthat::expect_equal(length(missing_species), 1)
  testthat::expect_true(is.numeric(missing_species))
  testthat::expect_equal(missing_species, round(missing_species))
  testthat::expect_true(missing_species >= 0)
  testthat::expect_true(is.finite(missing_species))

  # As function is mainly called for side effect, see
  # See https://style.tidyverse.org/functions.html#return
  invisible(missing_species)
}
