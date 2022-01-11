#' Check if an `empirical_island_clade` is valid.
#'
#' Check if an `empirical_island_clade` is valid.
#' Will \link{stop} if not.
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @seealso Use \link{check_ideal_island_clade} to check
#' if an `ideal_island_clade` is valid.
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
#' empirical_island <- daisie_data$empirical_island
#' #  First clade
#' empirical_island_clade <- empirical_island[[1]]
#' check_empirical_island_clade(empirical_island_clade)
#' @author Richèl J.C. Bilderbeek
#'
#' @export
check_empirical_island_clade <- function(empirical_island_clade) {
  testthat::expect_true(is.list(empirical_island_clade))
  testthat::expect_true("branching_times" %in% names(empirical_island_clade))
  testthat::expect_true("stac" %in% names(empirical_island_clade))
  testthat::expect_true("missing_species" %in% names(empirical_island_clade))
  # 'all_colonisations' can also be present

  # Could improve testing here
}
