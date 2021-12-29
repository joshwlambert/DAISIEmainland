#' Check if a `branching_times` is valid.
#'
#' Check if a `branching_times` is valid.
#' Will \link{stop} if not
#'
#' `branching_times` contains:
#'
#'  * for cladogenetic species: the island age
#'    and branching times of the radiation,
#'    including the stem age of the radiation
#'  * for non-endemic, non-endemic_MaxAge and
#'    Endemic anagenetic species:
#'    island age and stem age of the population/species
#'
#' A valid `branching_times` ...
#'
#'  * is numeric
#'  * has only finite values
#'  * has at least 1 element (which equals the island age)
#'  * has only positive non-zero values (although zero is accepted, to prevent
#'    false negatives by rounding off errors)
#'  * has a descending order
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @examples
#' check_branching_times(1.0)
#' check_branching_times(c(1.0, 0.25))
#' @author Richèl J.C. Bilderbeek
#' @export
check_branching_times <- function(branching_times) {
  testthat::expect_true(is.numeric(branching_times))
  testthat::expect_false(any(is.infinite(branching_times)))
  testthat::expect_true(length(branching_times) >= 1)
  testthat::expect_true(all(branching_times >= 0.0))

  # Must be in descending order
  testthat::expect_true(!is.unsorted(rev(branching_times)))

  # As function is mainly called for side effect, see
  # See https://style.tidyverse.org/functions.html#return
  invisible(branching_times)
}
