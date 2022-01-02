#' Formats simulation output into standard DAISIE list output.
#'
#' @inheritParams default_params_doc
#'
#' @return a `daisie_data` object,
#' as can be checked by \link{check_daisie_data}

#' @keywords internal
#' @author Joshua W. Lambert
format_to_daisie_data <- function(island_replicates,
                                  total_time,
                                  m) {
  if (1 == 2) {
    # Commented out temporarily for speed
    DAISIEmainland::check_island_replicates(island_replicates) # TODO: remove, #45
  }

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
    total_time = total_time,
    m = m)

  empirical_islands <- format_to_daisie_data_core(
    island_replicates = empirical_island_replicates,
    total_time = total_time,
    m = m)

  daisie_data <- list(ideal_islands = ideal_islands,
                      empirical_islands = empirical_islands)
  DAISIEmainland::check_daisie_data(daisie_data) # TODO: remove, #45
  daisie_data
}
