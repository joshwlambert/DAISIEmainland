#' Check if an `ideal_island_clade` is valid.
#'
#' Check if an `ideal_island_clade` is valid.
#' Will \link{stop} if not.
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @seealso Use \link{check_empirical_island_clade} to check
#' if an `empirical_island_clade` is valid.
#'
#' @examples
#' set.seed(
#'   2,
#'   kind = "Mersenne-Twister",
#'   normal.kind = "Inversion",
#'   sample.kind = "Rejection"
#' )
#' mainland_clade <- DAISIEmainland:::create_test_mainland_clade(
#'   mainland_scenario = 2
#' )
#' island_tbl <- DAISIEmainland:::sim_island(
#'   total_time = 1,
#'   island_pars = c(1, 1, 10, 1, 1),
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
#'
#' ideal_island <- daisie_data$ideal_island
#' #  First clade
#' ideal_island_clade <- ideal_island[[1]]
#' check_ideal_island_clade(ideal_island_clade)
#' @author Richèl J.C. Bilderbeek
#'
#' @export
check_ideal_island_clade <- function(ideal_island_clade) {
  testthat::expect_true(is.list(ideal_island_clade))
  testthat::expect_true("branching_times" %in% names(ideal_island_clade))
  testthat::expect_true("stac" %in% names(ideal_island_clade))
  testthat::expect_true("missing_species" %in% names(ideal_island_clade))
  # 'all_colonisations' can or cannot be present

  DAISIEmainland::check_branching_times(ideal_island_clade$branching_times)
  DAISIEmainland::check_stac(ideal_island_clade$stac)
  DAISIEmainland::check_missing_species(ideal_island_clade$missing_species)
  if ("all_colonisations" %in% names(ideal_island_clade)) {
    DAISIEmainland::check_all_colonisations(
      ideal_island_clade$all_colonisations)
  }
}
