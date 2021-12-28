#' Check if an `colonisations` is valid.
#'
#' Check if an `colonisations` is valid.
#' Will \link{stop} if not.
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
#' mainland_clade <- create_test_mainland_clade(
#'   mainland_scenario = 20
#' )
#' island <- sim_island(
#'   total_time = 1,
#'   island_pars = c(1, 1, 10, 12, 1),
#'   mainland = mainland_clade,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete")
#' colonisations <- island$ideal_island[[2]]$all_colonisations[[1]]
#' check_colonisations(colonisations)
#' @author Richèl J.C. Bilderbeek
#'
#' @export
check_colonisations <- function(colonisations) {
  testthat::expect_true(is.list(colonisations))
  testthat::expect_true("event_times" %in% names(colonisations))
  testthat::expect_true("species_type" %in% names(colonisations))
  DAISIEmainland::check_event_times(colonisations$event_times)
  DAISIEmainland::check_species_type(colonisations$species_type)

  # As function is mainly called for side effect, see
  # See https://style.tidyverse.org/functions.html#return
  invisible(colonisations)
}
