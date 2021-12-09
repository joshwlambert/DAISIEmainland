#' Determines if the island contains any recolonisations that survived to the
#' present
#'
#' @inheritParams default_params_doc
#' @return Boolean
#' @export
#'
#' @examples
#' \dontrun{
#' island <- DAISIEmainland::sim_island_with_mainland(
#'   total_time = 1,
#'   m = 100,
#'   island_pars = c(1,1,10,0.1,1),
#'   mainland_ex = 0.1,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete",
#'   replicates = 2,
#'   verbose = FALSE)
#' bool <- any_recols(island$ideal_islands[[1]])
#' }
any_recols <- function(island) {
  testit::assert(is.list(island))
  island <- island[-1]
  stacs <- unlist(lapply(island, "[[", "stac"))
  any_recols <- any(stacs == 3)
  return(any_recols)
}
