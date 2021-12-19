#' Check if an `ideal_daisie_data` is valid.
#' Will \link{stop} if not.
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
check_ideal_daisie_data <- function(ideal_daisie_data) {
  testthat::expect_true(is.list(ideal_daisie_data))

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
