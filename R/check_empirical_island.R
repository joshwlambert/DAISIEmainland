#' Check if an `empirical_island` is valid.
#'
#' Check if an `empirical_island` is valid.
#' Will \link{stop} if not.
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @seealso Use \link{check_ideal_island} to check if an `ideal_island`
#' is valid.
#'
#' @examples
#' set.seed(
#'   2,
#'   kind = "Mersenne-Twister",
#'   normal.kind = "Inversion",
#'   sample.kind = "Rejection"
#' )
#' mainland_clade <- DAISIEmainland:::create_test_mainland_clade(
#'   mainland_scenario = 2)
#' island <- DAISIEmainland:::sim_island(
#'   total_time = 1,
#'   empirical_island_pars = c(1, 1, 10, 1, 1),
#'   mainland = mainland_clade,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete")
#'
#' check_empirical_island(island$empirical_island)
#' @author Richèl J.C. Bilderbeek
#'
#' @export
check_empirical_island <- function(empirical_island) {
  # Data types have the same form
  DAISIEmainland::check_ideal_island(empirical_island)
}
