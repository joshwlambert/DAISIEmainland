#' Calculates the delta colonisation through time (CTT) statistic between the
#' ideal and empirical data sets simulated for each replicate
#'
#' @inheritParams default_params_doc
#'
#' @return A vector of numerics
#' @author Joshua W. Lambert
calc_ctt <- function(daisie_mainland_data) {
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
    }
  )

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

  delta_ctt_vec <- c()
  for (i in seq_along(daisie_mainland_data$ideal_multi_daisie_data)) {
    # normalise data
    ideal_col_times <- ideal_col_times_list[[i]]
    ideal_col_times <- sort(ideal_col_times, decreasing = TRUE)
    ideal_norm_col_times <- 1 - ideal_col_times / max(ideal_col_times)
    ideal_lineages <- seq_along(ideal_norm_col_times)
    ideal_lineages_norm <- ideal_lineages / max(ideal_lineages)
    testit::assert(all(ideal_norm_col_times >= 0 & ideal_norm_col_times <= 1))
    testit::assert(all(ideal_lineages_norm >= 0 & ideal_lineages_norm <= 1))
    ideal_norm_col_times <- c(ideal_norm_col_times, 1) # nolint check with Richel why this is needed
    ideal_lineages_norm <- c(ideal_lineages_norm, 1) # nolint check with Richel why this is needed

    empirical_col_times <- empirical_col_times_list[[i]]
    empirical_col_times <- sort(empirical_col_times, decreasing = TRUE)
    empirical_norm_col_times <-
      1 - empirical_col_times / max(empirical_col_times)
    empirical_lineages <- seq_along(empirical_norm_col_times)
    empirical_lineages_norm <- empirical_lineages / max(empirical_lineages)
    testit::assert(all(empirical_norm_col_times >= 0 &
      empirical_norm_col_times <= 1))
    testit::assert(all(empirical_lineages_norm >= 0 &
      empirical_lineages_norm <= 1))
    empirical_norm_col_times <- c(empirical_norm_col_times, 1) # nolint check with Richel why this is needed
    empirical_lineages_norm <- c(empirical_lineages_norm, 1) # nolint check with Richel why this is needed

    # calculate delta colonisation through time
    delta_ctt <- nLTT::nltt_diff_exact_norm_brts(
      b_times_n = ideal_norm_col_times,
      lineages_n = ideal_lineages_norm,
      b_times2_n = empirical_norm_col_times,
      lineages2_n = empirical_lineages_norm,
      distance_method = "abs"
    )

    delta_ctt_vec <- c(delta_ctt_vec, delta_ctt)
  }
  delta_ctt_vec
}
