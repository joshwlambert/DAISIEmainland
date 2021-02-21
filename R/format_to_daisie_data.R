#' Formats simulation output into standard DAISIE list output.
#'
#' @inheritParams default_params_doc
#'
#' @return List with DAISIE simulation output
#' @keywords internal
format_to_daisie_data <- function(
  island_replicates,
  time,
  m) {

  ideal_island_replicates <- list()
  empirical_island_replicates <- list()
  for (i in seq_along(island_replicates)) {
    ideal_island_replicates[[i]] <- list()
    empirical_island_replicates[[i]] <- list()
    for (j in seq_along(island_replicates[[i]])) {
      ideal_island_replicates[[i]][[j]] <-
        island_replicates[[i]][[j]]$ideal_island
      empirical_island_replicates[[i]][[j]] <-
        island_replicates[[i]][[j]]$empirical_island
    }
  }

  ideal_islands <- format_to_daisie_data_core(
    island_replicates = ideal_island_replicates,
    time = time,
    m = m)

  empirical_islands <- format_to_daisie_data_core(
    island_replicates = empirical_island_replicates,
    time = time,
    m = m)

  return(list(ideal_islands = ideal_islands,
              empirical_islands = empirical_islands))
}
