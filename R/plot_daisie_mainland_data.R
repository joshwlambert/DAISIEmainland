#' Plot the `daisie_mainland_data` for a certain replicate
#' @inheritParams default_params_doc
#'
#' @return a `ggplot2`
#'
#' @examples
#' daisie_mainland_data <- sim_island_with_mainland(
#'   total_time = 1,
#'   m = 10,
#'   island_pars = c(1, 1, 10, 0.1, 1),
#'   mainland_ex = 1,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "undiscovered",
#'   replicates = 10,
#'   verbose = FALSE
#' )
#'
#' plot_daisie_mainland_data(
#'   daisie_mainland_data,
#'   replicate_index = 1
#' )
#' @author Richèl J.C. Bilderbeek
#' @export
plot_daisie_mainland_data <- function(
  daisie_mainland_data,
  replicate_index
) {
  testthat::expect_true(replicate_index >= 1)
  testthat::expect_true(
    replicate_index <= length(daisie_mainland_data$ideal_multi_daisie_data))
  testthat::expect_equal(
    length(daisie_mainland_data$ideal_multi_daisie_data),
    length(daisie_mainland_data$empirical_multi_daisie_data))

  ideal_multi_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[
    replicate_index]]
  empirical_multi_daisie_data <- daisie_mainland_data$empirical_multi_daisie_data[[
    replicate_index]]


  ggplot2::ggplot()
}

