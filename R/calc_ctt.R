#' Calculates the delta colonisation through time (CTT) statistic between the
#' ideal and empirical data sets simulated for each replicate
#'
#' @inheritParams default_params_doc
#'
#' @return A vector of numerics
#' @author Joshua W. Lambert
calc_ctt <- function(daisie_data) {

  delta_ctt_vec <- c()

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

    # normalise data
    ideal_col_times <- sort(ideal_col_times, decreasing = TRUE)
    ideal_norm_col_times <- 1 - ideal_col_times / max(ideal_col_times)
    ideal_lineages <- seq_along(ideal_norm_col_times)
    ideal_lineages_norm <- ideal_lineages / max(ideal_lineages)
    testit::assert(all(ideal_norm_col_times >= 0 & ideal_norm_col_times <= 1))
    testit::assert(all(ideal_lineages_norm >= 0 & ideal_lineages_norm <= 1))
    ideal_norm_col_times <- c(ideal_norm_col_times, 1) #check with Thijs why this is needed
    ideal_lineages_norm <- c(ideal_lineages_norm, 1) #check with Thijs why this is needed

    empirical_col_times <- sort(empirical_col_times, decreasing = TRUE)
    empirical_norm_col_times <-
      1 - empirical_col_times / max(empirical_col_times)
    empirical_lineages <- seq_along(empirical_norm_col_times)
    empirical_lineages_norm <- empirical_lineages / max(empirical_lineages)
    testit::assert(all(empirical_norm_col_times >= 0 &
                         empirical_norm_col_times <= 1))
    testit::assert(all(empirical_lineages_norm >= 0 &
                         empirical_lineages_norm <= 1))
    empirical_norm_col_times <- c(empirical_norm_col_times, 1) #check with Thijs why this is needed
    empirical_lineages_norm <- c(empirical_lineages_norm, 1) #check with Thijs why this is needed

    # calculate delta colonisation through time
    delta_ctt <- nLTT::nltt_diff_exact_norm_brts(
      b_times_n = ideal_norm_col_times,
      lineages_n = ideal_lineages_norm,
      b_times2_n = empirical_norm_col_times,
      lineages2_n = empirical_lineages_norm,
      distance_method = "abs")

    delta_ctt_vec <- c(delta_ctt_vec, delta_ctt)
  }

  return(delta_ctt_vec)
}
