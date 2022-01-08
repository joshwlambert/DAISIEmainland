#' Calculates the percent of max age species on the island for each replicate
#'
#' @inheritParams default_params_doc
#'
#' @return A list of two numeric vectors
#' @author Joshua W. Lambert
calc_max_age_percent <- function(daisie_mainland_data) {

  #extract island age from one of the simulations
  time <- daisie_mainland_data$ideal_multi_daisie_data[[1]][[1]]$island_age
  max_age <- time - 1e-5

  ideal_max_age_vec <- c()
  empirical_max_age_vec <- c()

  testit::assert(identical(
    length(daisie_mainland_data$ideal_multi_daisie_data),
    length(daisie_mainland_data$empirical_multi_daisie_data)
  ))

  for (i in seq_along(daisie_mainland_data$ideal_multi_daisie_data)) {
    ideal_col_times <- c()
    empirical_col_times <- c()

    for (j in 2:length(daisie_mainland_data$ideal_multi_daisie_data[[i]])) {
      ideal_stac <- daisie_mainland_data$ideal_multi_daisie_data[[i]][[j]]$stac
      if (ideal_stac %in% c(1, 2, 4, 5, 6)) {
        ideal_col_times <- c(
          ideal_col_times,
          daisie_mainland_data$ideal_multi_daisie_data[[i]][[j]]$branching_times[2]
        )
      } else {
        for (k in seq_along(daisie_mainland_data$ideal_multi_daisie_data[[i]][[j]]$all_colonisations)) {
          ideal_col_times <- c(
            ideal_col_times,
            daisie_mainland_data$ideal_multi_daisie_data[[i]][[j]]$all_colonisations[[k]]$event_times[[2]]
          )
        }
      }
    }

    for (j in 2:length(daisie_mainland_data$empirical_multi_daisie_data[[i]])) {
      empirical_stac <- daisie_mainland_data$empirical_multi_daisie_data[[i]][[j]]$stac
      if (empirical_stac %in% c(1, 2, 4, 5, 6)) {
        empirical_col_times <- c(
          empirical_col_times,
          daisie_mainland_data$empirical_multi_daisie_data[[i]][[j]]$branching_times[2]
        )
      } else {
        for (k in seq_along(daisie_mainland_data$empirical_multi_daisie_data[[i]][[j]]$all_colonisations)) {
          empirical_col_times <- c(
            empirical_col_times,
            daisie_mainland_data$ideal_multi_daisie_data[[i]][[j]]$all_colonisations[[k]]$event_times[[2]]
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
