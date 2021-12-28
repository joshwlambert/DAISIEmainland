#' Check if an `ideal_island` is valid.
#'
#' Check if an `ideal_island` is valid.
#' Will \link{stop} if not.
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @seealso Use \link{check_empirical_island} to check if an `empirical_island`
#' is valid.
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
#'   ideal_island_pars = c(1, 1, 10, 1, 1),
#'   mainland = mainland_clade,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete")
#'
#' check_ideal_island(island$ideal_island)
#' @author Richèl J.C. Bilderbeek
#'
#' @export
check_ideal_island <- function(ideal_island) {
  testthat::expect_true(is.list(ideal_island))

  for (i in seq_along(ideal_island)) {
    ideal_island_clade <- ideal_island[[i]]
    tryCatch(
      DAISIEmainland::check_ideal_island_clade(ideal_island_clade),
      error = function(e) {
        stop(
          "Error in clade #", i, " of 'ideal_island', \n",
          "with error message: ", e$message
        )
      }
    )
  }
  # As function is mainly called for side effect, see
  # See https://style.tidyverse.org/functions.html#return
  invisible(ideal_island)
}
