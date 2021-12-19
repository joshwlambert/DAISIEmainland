#' Check if a `daisie_data` is valid.
#' Will \link{stop} if not.
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @examples
#' daisie_data <- sim_island_with_mainland(
#'   total_time = 1,
#'   m = 100,
#'   island_pars = c(1, 1, 50, 0.1, 1),
#'   mainland_ex = 0.5,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete",
#'   replicates = 1,
#'   verbose = FALSE
#' )
#' check_daisie_datalist(daisie_data)
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
check_daisie_data <- function(daisie_data) {
  testthat::expect_true(is.list(daisie_data))
  testthat::expect_true("ideal_islands" %in% names(daisie_data))
  testthat::expect_true("empirical_islands" %in% names(daisie_data))
  DAISIEmainland::check_ideal_daisie_data(
    ideal_daisie_data = daisie_data$ideal_islands)
  DAISIEmainland::check_empirical_daisie_data(
    empirical_daisie_data = daisie_data$empirical_islands)

  # As function is mainly called for side effect, see
  # See https://style.tidyverse.org/functions.html#return
  invisible(daisie_data)
}
