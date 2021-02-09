#' Formats simulation output into standard DAISIE list output.
#'
#' @inheritParams default_params_doc
#'
#' @return List with DAISIE simulation output
#' @keywords internal
format_data <- function(island_replicates,
                        time,
                        m,
                        verbose = TRUE) {

  ideal_island_replicates <- list()
  reality_island_replicates <- list()
  for (i in seq_along(island_replicates)) {
    ideal_island_replicates[[i]] <- list()
    reality_island_replicates[[i]] <- list()
    for (j in seq_along(island_replicates[[i]])) {
      ideal_island_replicates[[i]][[j]] <-
        island_replicates[[i]][[j]]$ideal_island
      reality_island_replicates[[i]][[j]] <-
        island_replicates[[i]][[j]]$reality_island
    }
  }

  ideal_islands <- format_data_core(
    island_replicates = ideal_island_replicates,
    time = time,
    m = m,
    verbose = verbose)

  reality_islands <- format_data_core(
    island_replicates = reality_island_replicates,
    time = time,
    m = m,
    verbose = verbose)

  return(list(ideal_islands = ideal_islands,
              reality_islands = reality_islands))
}
