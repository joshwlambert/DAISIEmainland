#' Check if a `daisie_datalist` (i.e. a `DAISIE` `datalist`) is valid.
#' Will \link{stop} if not.
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
check_daisie_datalist <- function(daisie_datalist) {
  testthat::expect_true(is.list(daisie_datalist))

  for (i in seq_along(daisie_datalist)) {
    daisie_datalist <- daisie_datalist[[i]]
    # Do add the index of the element that the error occurs in
    tryCatch(
      DAISIEmainland::check_daisie_datalist(daisie_datalist),
      error = function(e) {
        stop("Error in 'daisie_datalist[[", i, "]]:" , e$message)
      }
    )
  }
}
