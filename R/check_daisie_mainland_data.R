#' Check if a `daisie_mainland_data` is valid.
#' Will \link{stop} if not.
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @examples
#' daisie_mainland_data <- sim_island_with_mainland(
#'   total_time = 1,
#'   m = 100,
#'   island_pars = c(1, 1, 50, 0.1, 1),
#'   mainland_ex = 0.5,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete",
#'   replicates = 1,
#'   verbose = FALSE
#' )
#' check_daisie_mainland_data(daisie_mainland_data)
#'
#' @author Richèl J.C. Bilderbeek, Joshua W. Lambert
#'
#' @export
check_daisie_mainland_data <- function(daisie_mainland_data) {
  testit::assert(is.list(daisie_mainland_data))
  testit::assert("ideal_multi_daisie_data" %in% names(daisie_mainland_data))
  testit::assert("empirical_multi_daisie_data" %in% names(daisie_mainland_data))
  DAISIEmainland::check_multi_daisie_data(
    multi_daisie_data = daisie_mainland_data$ideal_multi_daisie_data
  )
  DAISIEmainland::check_multi_daisie_data(
    multi_daisie_data = daisie_mainland_data$empirical_multi_daisie_data
  )
  invisible(daisie_mainland_data)
}
