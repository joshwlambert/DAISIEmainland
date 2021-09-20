#' Calculates the percent of max age species on the island for each replicate
#'
#' @inheritParams default_params_doc
#'
#' @return A list of two numeric vectors
#' @author Joshua W. Lambert
calc_max_age_percent <- function(daisie_data) {

  #extract island age from one of the simulations
  time <- daisie_data$ideal_islands[[1]][[1]]$island_age
  max_age <- time - 1e-5

  ideal_max_age_vec <- c()
  empirical_max_age_vec <- c()

  testit::assert(length(daisie_data$ideal_island) ==
                   length(daisie_data$empirical_island))

  for (i in seq_along(daisie_data$ideal_islands)) {
    ideal_col_times <- c()
    empirical_col_times <- c()

    for (j in 2:length(daisie_data$ideal_islands[[i]])) {
      ideal_stac <- daisie_data$ideal_islands[[i]][[j]]$stac
      if (ideal_stac == 1 || ideal_stac == 2 || ideal_stac == 4 ||
          ideal_stac == 5 || ideal_stac == 6) {
        ideal_col_times <- c(
          ideal_col_times,
          daisie_data$ideal_islands[[i]][[j]]$branching_times[2]
        )
      } else {
        for (k in seq_along(daisie_data$ideal_islands[[i]][[j]]$all_colonisations)) {
          ideal_col_times <- c(
            ideal_col_times,
            daisie_data$ideal_islands[[i]][[j]]$all_colonisations[[k]]$event_times[[2]]
          )
        }
      }
    }

    for (j in 2:length(daisie_data$empirical_islands[[i]])) {
      empirical_stac <- daisie_data$empirical_islands[[i]][[j]]$stac
      if (empirical_stac == 1 || empirical_stac == 2 || empirical_stac == 4 ||
          empirical_stac == 5 || empirical_stac == 6) {
        empirical_col_times <- c(
          empirical_col_times,
          daisie_data$empirical_islands[[i]][[j]]$branching_times[2]
        )
      } else {
        for (k in seq_along(daisie_data$empirical_islands[[i]][[j]]$all_colonisations)) {
          empirical_col_times <- c(
            empirical_col_times,
            daisie_data$ideal_islands[[i]][[j]]$all_colonisations[[k]]$event_times[[2]]
          )
        }
      }
    }
    #calc max age percentage
    ideal_max_age <- (length(which(ideal_col_times == max_age)) /
      length(ideal_col_times)) * 100
    testit::assert(ideal_max_age == 0)

    empirical_max_age <- (length(which(empirical_col_times == max_age)) /
      length(empirical_col_times)) * 100

    ideal_max_age_vec <- c(ideal_max_age_vec, ideal_max_age)
    empirical_max_age_vec <- c(empirical_max_age_vec, empirical_max_age)

  }

  max_age_percent_list <- list(ideal_max_age = ideal_max_age_vec,
                               empirical_max_age = empirical_max_age_vec)

  return(max_age_percent_list)
}
