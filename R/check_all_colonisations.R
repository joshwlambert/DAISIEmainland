#' Check if a `all_colonisations` is valid.
#'
#' Check if a `all_colonisations` is valid.
#' Will \link{stop} if not
#'
#' `all_colonisations` contains:
#'
#'  * for cladogenetic species: the island age
#'    and branching times of the radiation,
#'    including the stem age of the radiation
#'  * for non-endemic, non-endemic_MaxAge and
#'    Endemic anagenetic species:
#'    island age and stem age of the population/species
#'
#' A valid `all_colonisations` ...
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
#' check_all_colonisations(1.0)
#' check_all_colonisations(c(1.0, 0.25))
#' @author Richèl J.C. Bilderbeek
#' @export
check_all_colonisations <- function(all_colonisations) {
  testthat::expect_true(is.list(all_colonisations))
  testthat::expect_true(length(all_colonisations) >= 1)

  for (i in seq_along(all_colonisations)) {
    colonisations <- all_colonisations[[i]]
    tryCatch(
      DAISIEmainland::check_colonisations(colonisations),
      error = function(e) {
        stop(
          "Error in clade #", i, " of 'all_colonisations', \n",
          "with error message: ", e$message
        )
      }
    )
  }

  # As function is mainly called for side effect, see
  # See https://style.tidyverse.org/functions.html#return
  invisible(all_colonisations)
}
