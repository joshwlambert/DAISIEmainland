#' Check if an `multi_mainland_clade` is valid.
#' Will \link{stop} if not.
#'
#' An `multi_mainland_clade` ...
#'
#'  * is a \link{list} of `mainland_clade`, see \link{check_mainland_clade}
#'  * has at least 1 element
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @examples
#' multi_mainland_clade <- sim_mainland(
#'   total_time = 1,
#'   m = 10,
#'   mainland_ex = 0.5
#' )
#' check_multi_mainland_clade(multi_mainland_clade)
#'
#' @author Richèl J.C. Bilderbeek, Joshua W. Lambert
#'
#' @export
check_multi_mainland_clade <- function(multi_mainland_clade) {

  testit::assert(is.list(multi_mainland_clade))

  for (i in seq_along(multi_mainland_clade)) {
    mainland_clade <- multi_mainland_clade[[i]]
    # Do add the index of the element that the error occurs in
    tryCatch(
      DAISIEmainland::check_mainland_clade(mainland_clade),
      error = function(e) {
        stop("Error in 'multi_mainland_clade[[", i, "]]: ", e$message)
      }
    )
  }
}
