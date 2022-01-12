#' Check if an `mainland_clade` is valid.
#'
#' Check if an `mainland_clade` is valid.
#' Will \link{stop} if not.
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @examples
#' set.seed(
#'   2,
#'   kind = "Mersenne-Twister",
#'   normal.kind = "Inversion",
#'   sample.kind = "Rejection"
#' )
#'
#' multi_mainland_clade <- DAISIEmainland::sim_mainland(
#'   total_time = 1,
#'   m = 2,
#'   mainland_ex = 1
#' )
#'
#' mainland_clade <- multi_mainland_clade[[1]]
#' check_mainland_clade(mainland_clade)
#' @author Joshua W. Lambert, Richèl J.C. Bilderbeek
#'
#' @export
check_mainland_clade <- function(mainland_clade) {
  testit::assert(is.data.frame(mainland_clade))
  testit::assert(identical(ncol(mainland_clade), 7L))
  testit::assert(identical(
    names(mainland_clade),
    c(
      "spec_id", "main_anc_id", "spec_type", "branch_code",
      "branch_t", "spec_origin_t", "spec_ex_t"
    )
  ))

  invisible(mainland_clade)
}
