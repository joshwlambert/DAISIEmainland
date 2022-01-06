group_daisie_data <- function(island_replicates,
                              total_time,
                              m) {

  ideal_daisie_data <- list()
  empirical_daisie_data <- list()
  for (i in seq_along(island_replicates)) {
    ideal_daisie_data[[i]] <- list()
    empirical_daisie_data[[i]] <- list()
    for (j in seq_along(island_replicates[[i]])) {
      ideal_daisie_data[[i]][[j]] <-
        island_replicates[[i]][[j]]$ideal_island
      empirical_daisie_data[[i]][[j]] <-
        island_replicates[[i]][[j]]$empirical_island
    }
  }

  return(list(ideal_daisie_data = ideal_daisie_data,
              empirical_daisie_data = empirical_daisie_data))
}
