#' Determine if a `daisie_datalist` is valid.
#'
#' @inheritParams default_params_doc
#'
#' @return \link{TRUE} if the `daisie_datalist` is valid.
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
is_daisie_datalist <- function(
  daisie_datalist,
  verbose = FALSE
) {
  result <- FALSE
  tryCatch({
    DAISIEmainland::check_daisie_datalist(
      daisie_datalist = daisie_datalist
    )
    result <- TRUE
  }, error = function(e) {
    if (verbose) message(e$message)
  }
  )
  result
}
