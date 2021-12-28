#' Check if an `island` is valid.
#'
#' Check if an `island` is valid.
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
#' mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
#' island <- sim_island(
#'   total_time = 1,
#'   island_pars = c(1, 1, 10, 1, 1),
#'   mainland = mainland_clade,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete")
#'
#' check_island(island)
#' @author Richèl J.C. Bilderbeek
#'
#' @export
check_island <- function(island) {
  testthat::expect_true(is.list(island))
  testthat::expect_true("ideal_island" %in% names(island))
  testthat::expect_true("empirical_island" %in% names(island))
  DAISIEmainland::check_ideal_island(island$ideal_island)
  DAISIEmainland::check_empirical_island(island$empirical_island)

  # As function is mainly called for side effect, see
  # See https://style.tidyverse.org/functions.html#return
  invisible(island)
}
