#' Title
#'
#' @param daisie_data stub
#'
#' @return
#' @export
#' @author Joshua W. Lambert
calc_endemic_ratio <- function(daisie_data) {

  endemic_ratio_list <- list()

  testit::assert(length(daisie_data$ideal_island) ==
                   length(daisie_data$empirical_island))

  for (i in seq_along(daisie_data$ideal_islands)) {

    ideal_stacs <- c()
    empirical_stacs <- c()

    for (j in 2:length(daisie_data$ideal_islands[[i]])) {
      ideal_stac <- daisie_data$ideal_islands[[i]][[j]]$stac
      if (ideal_stac == 1 || ideal_stac == 2 || ideal_stac == 4 ||
          ideal_stac == 5 || ideal_stac == 6) {
        ideal_stacs <- c(
          ideal_stacs,
          daisie_data$ideal_islands[[i]][[j]]$branching_times[2]
        )
      } else {
        for (l in seq_along(daisie_data$ideal_islands[[i]][[k]]$all_colonisations)) {
          ideal_col_times <- c(
            ideal_col_times,
            daisie_data$ideal_islands[[i]][[k]]$all_colonisations[[l]]$event_times[[2]]
          )
        }
      }
    }

    for (k in 2:length(daisie_data$empirical_islands[[i]])) {
      empirical_stac <- daisie_data$empirical_islands[[i]][[k]]$stac
      if (empirical_stac == 1 || empirical_stac == 2 || empirical_stac == 4 ||
          empirical_stac == 5 || empirical_stac == 6) {
        empirical_col_times <- c(
          empirical_col_times,
          daisie_data$empirical_islands[[i]][[k]]$branching_times[2]
        )
      } else {
        for (l in seq_along(daisie_data$ideal_islands[[i]][[k]]$all_colonisations)) {
          empirical_col_times <- c(
            empirical_col_times,
            daisie_data$ideal_islands[[i]][[k]]$all_colonisations[[l]]$event_times[[2]]
          )
        }
      }
    }

    # calculate delta colonisation through time
    endemic_ratio <- NULL

    endemic_ratio_list[[i]] <- endemic_ratio
  }

  return(endemic_ratio_list)
}
