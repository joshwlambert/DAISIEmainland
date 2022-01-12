#' Check if an `colonisations` is valid.
#'
#' Check if an `colonisations` is valid.
#' Will \link{stop} if not.
#'
#' A `colonisations` contains:
#'
#'  * a `species_type`, which is the origin of the clade,
#'    which can be anagenetic, cladogenetic or immigration.
#'    See \link{check_species_type} for details
#'  * a `event_times`, which holds events such as the island age,
#'    stem age of the species and -for cladogenetic species only-,
#'    the branching times of the radiation.
#'    see \link{check_event_times}
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
#' island_tbl <- DAISIEmainland:::sim_island(
#'   total_time = 1,
#'   island_pars = c(1, 1, 10, 12, 1),
#'   mainland = mainland_clade,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete"
#' )
#' daisie_data <- DAISIEmainland:::create_daisie_data(
#'   total_time = 1,
#'   island_tbl = island_tbl,
#'   mainland_clade = mainland_clade,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete"
#' )
#' colonisations <- daisie_data$ideal_island[[2]]$all_colonisations[[1]]
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

  invisible(colonisations)
}
