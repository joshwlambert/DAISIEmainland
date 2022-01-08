#' Title
#'
#' @param multi_daisie_data
#' @param total_time
#' @param m
#'
#' @return
#' @export
#'
#' @examples
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
