#' Check if an `ideal_daisie_data` is valid.
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
#' check_ideal_daisie_data(daisie_data$ideal_islands)
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
check_ideal_daisie_data <- function(ideal_daisie_data) {
  testthat::expect_true(is.list(ideal_daisie_data))

  if (DAISIEmainland::is_daisie_datalist(ideal_daisie_data)) {
    stop(
      "'ideal_daisie_data' must be a list of 'DAISIE::datalist', ",
      "got a 'DAISIE::datalist' instead"
    )
  }

  for (i in seq_along(ideal_daisie_data)) {
    daisie_datalist <- ideal_daisie_data[[i]]
    # Do add the index of the element that the error occurs in
    tryCatch(
      DAISIEmainland::check_daisie_datalist(daisie_datalist),
      error = function(e) {
        stop("Error in 'ideal_daisie_data[[", i, "]]: ", e$message)
      }
    )
  }
}
