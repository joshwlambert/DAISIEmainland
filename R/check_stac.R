#' Check if a `stac` is valid.
#'
#' Check if a `stac` is valid.
#' Will \link{stop} if not
#'
#' A valid `stac` ...
#'
#'  * is numeric
#'  * has one element
#'  * the element has a whole-number value in range `[0, 6]`
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @seealso Use \link{stac_to_str} to convert a `stac` to a string.
#'
#' @examples
#' check_stac(0)
#' check_stac(1)
#' check_stac(6)
#' @author Richèl J.C. Bilderbeek
#' @export
check_stac <- function(stac) {
  testthat::expect_equal(length(stac), 1)
  testthat::expect_true(is.numeric(stac))
  testthat::expect_equal(stac, round(stac))
  testthat::expect_true(stac >= 0)
  testthat::expect_true(stac <= 6)

  # As function is mainly called for side effect, see
  # See https://style.tidyverse.org/functions.html#return
  invisible(stac)
}
