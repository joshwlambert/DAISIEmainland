#' Converts simulation output into island output
#'
#' @inheritParams default_params_doc
#'
#' @return List with the island information, composed of
#' branching times of extant species, status of species on
#' the island and number of missing species.
#' @keywords internal
#' @author Joshua W. Lambert
create_island <- function(total_time,
                          island_spec,
                          mainland_clade,
                          mainland_sample_prob) {

  testit::assert(is.numeric(total_time))
  testit::assert(is.data.frame(island_spec))
  testit::assert(ncol(island_spec) == 7)
  testit::assert(is.data.frame(mainland_clade))
  testit::assert(ncol(mainland_clade) == 7)
  testit::assert(is.numeric(mainland_sample_prob))

  if (nrow(island_spec) == 0) {
    ideal_island <- empirical_island <-
      list(list(
        branching_times = total_time,
        stac = 0,
        missing_species = 0))
  } else {
    islands <- create_non_empty_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob)
    ideal_island <- islands$ideal_island
    empirical_island <- islands$empirical_island
  }
  return(list(ideal_island = ideal_island,
              empirical_island = empirical_island))
}
