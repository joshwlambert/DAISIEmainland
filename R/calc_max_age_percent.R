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

  testit::assert(identical(
    length(daisie_mainland_data$ideal_multi_daisie_data),
    length(daisie_mainland_data$empirical_multi_daisie_data)
  ))

  # extract multi daisie data from daisie mainland data
  ideal_multi_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data
  empirical_multi_daisie_data <-
    daisie_mainland_data$empirical_multi_daisie_data
  # remove daisie meta data from each daisie_data
  ideal_multi_daisie_data <- lapply(ideal_multi_daisie_data, function(x) {
    x[-1]
  })
  empirical_multi_daisie_data <- lapply(
    empirical_multi_daisie_data,
    function(x) {
      x[-1]
    })

  # extract ideal colonisation times
  ideal_col_times <- lapply(ideal_multi_daisie_data, function(x) {
    lapply(x, function(y) {
      if (y$stac %in% c(1, 2, 4, 5, 6)) {
        y$branching_times[2]
      } else {
        lapply(y$all_colonisations, function(z) {
          z$event_times[2]
        })
      }
    })
  })

  # extract empirical colonisation times
  empirical_col_times <- lapply(empirical_multi_daisie_data, function(x) {
    lapply(x, function(y) {
      if (y$stac %in% c(1, 2, 4, 5, 6)) {
        y$branching_times[2]
      } else {
        lapply(y$all_colonisations, function(z) {
          z$event_times[2]
        })
      }
    })
  })

  # create a vector of colonisation times for each replicate
  ideal_col_times_list <- lapply(ideal_col_times, unlist)
  empirical_col_times_list <- lapply(empirical_col_times, unlist)

  ideal_max_age_vec <- c()
  empirical_max_age_vec <- c()
  for (i in seq_along(daisie_mainland_data$ideal_multi_daisie_data)) {
    ideal_col_times <- ideal_col_times_list[[i]]
    empirical_col_times <- empirical_col_times_list[[i]]
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

  max_age_percent_list
}
