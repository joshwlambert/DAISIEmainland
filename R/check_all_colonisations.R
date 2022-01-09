#' Check if a `all_colonisations` is valid.
#'
#' Check if a `all_colonisations` is valid.
#' Will \link{stop} if not
#'
#' `all_colonisations` is a list of one or more colonisations,
#' where each element is checked by \link{check_colonisations}.
#' See \link{check_colonisations} for more details.
#'
#' A valid `all_colonisations` ...
#'
#'  * is a list of one or more `colonisations`,
#'    where each `colonisations` is checked by \link{check_colonisations}
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @examples
#' set.seed(
#'   2,
#'   kind = "Mersenne-Twister",
#'   normal.kind = "Inversion",
#'   sample.kind = "Rejection"
#' )
#' mainland_clade <- DAISIEmainland:::create_test_mainland_clade(
#'   mainland_scenario = 20
#' )
#' island <- DAISIEmainland:::sim_island(
#'   total_time = 1,
#'   island_pars = c(1, 1, 10, 12, 1),
#'   mainland = mainland_clade,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete")
#' all_colonisations <- island$ideal_island[[2]]$all_colonisations
#' check_all_colonisations(all_colonisations)
#'
#' @author Richèl J.C. Bilderbeek
#' @export
check_all_colonisations <- function(all_colonisations) {
  #TODO I think this functionality should be merged into check_daisie_data
  #as it is part of the daisie_data object
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
  invisible(all_colonisations)
}
