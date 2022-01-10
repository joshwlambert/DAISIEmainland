#' Check if an `multi_daisie_data` is valid for both `ideal_multi_daisie_data`
#' and `empirical_multi_daisie_data`.
#' Will \link{stop} if not.
#'
#' An `multi_daisie_data` ...
#'
#'  * is a \link{list} of `daisie_data`, see \link{check_daisie_data}
#'  * has at least 1 element
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
#' check_multi_daisie_data(daisie_mainland_data$ideal_multi_daisie_data)
#' check_multi_daisie_data(daisie_mainland_data$empirical_multi_daisie_data)
#'
#' @author Richèl J.C. Bilderbeek, Joshua W. Lambert
#'
#' @export
check_multi_daisie_data <- function(multi_daisie_data) {

  testit::assert(is.list(multi_daisie_data))

  if (DAISIEmainland::is_daisie_data(multi_daisie_data)) {
    stop(
      "'multi_daisie_data' must be a list of 'daisie_data', ",
      "got a 'daisie_data' instead"
    )
  }

  for (i in seq_along(multi_daisie_data)) {
    daisie_data <- multi_daisie_data[[i]]
    # Do add the index of the element that the error occurs in
    tryCatch(
      DAISIEmainland::check_daisie_data(daisie_data),
      error = function(e) {
        stop("Error in 'multi_daisie_data[[", i, "]]: ", e$message)
      }
    )
  }
  invisible(multi_daisie_data)
}
