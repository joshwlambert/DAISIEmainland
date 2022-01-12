#' Determine if a `daisie_data` is valid.
#'
#' @inheritParams default_params_doc
#'
#' @return \link{TRUE} if the `daisie_data` is valid.
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
is_daisie_data <- function(daisie_data,
                           verbose = FALSE) {
  result <- FALSE
  tryCatch({
      DAISIEmainland::check_daisie_data(
        daisie_data = daisie_data
      )
      result <- TRUE
    },
    error = function(e) {
      if (verbose) message(e$message)
    }
  )
  result
}
