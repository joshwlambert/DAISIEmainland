#' Check if an `island_tbl` is valid.
#'
#' Check if an `island_tbl` is valid.
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
#' mainland_clade <- DAISIEmainland:::create_test_mainland_clade(
#'   mainland_scenario = 2
#' )
#' island_tbl <- DAISIEmainland:::sim_island(
#'   total_time = 1,
#'   island_pars = c(1, 1, 10, 1, 1),
#'   mainland = mainland_clade,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete"
#' )
#'
#' check_island_tbl(island_tbl)
#' @author Richèl J.C. Bilderbeek, Joshua W. Lambert
#'
#' @export
check_island_tbl <- function(island_tbl) {
  testit::assert(is.data.frame(island_tbl))
  testit::assert(identical(ncol(island_tbl), 7L))
  testit::assert(identical(
    names(island_tbl),
    c(
      "spec_id", "main_anc_id", "col_t", "spec_type",
      "branch_code", "branch_t", "ana_origin"
    )
  ))

  invisible(island_tbl)
}
