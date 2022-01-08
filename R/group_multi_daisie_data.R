#' Takes mixed multi_daisie_data produced by the looping over `sim_island` in
#' `sim_island_with_mainland` and groups the ideal and the empirical
#' `multi_daisie_data` together
#'
#' @inheritParams default_params_doc
#'
#' @return A list of two `multi_daisie_data` elements
group_multi_daisie_data <- function(multi_daisie_data,
                                    total_time,
                                    m) {
  ideal_multi_daisie_data <- list()
  empirical_multi_daisie_data <- list()
  for (i in seq_along(multi_daisie_data)) {
    ideal_multi_daisie_data[[i]] <- list()
    empirical_multi_daisie_data[[i]] <- list()
    for (j in seq_along(multi_daisie_data[[i]])) {
      ideal_multi_daisie_data[[i]][[j]] <-
        multi_daisie_data[[i]][[j]]$ideal_island
      empirical_multi_daisie_data[[i]][[j]] <-
        multi_daisie_data[[i]][[j]]$empirical_island
    }
  }

  return(list(ideal_multi_daisie_data = ideal_multi_daisie_data,
              empirical_multi_daisie_data = empirical_multi_daisie_data))
}
