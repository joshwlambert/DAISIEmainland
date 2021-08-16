#' Title
#'
#' @return
#' @export
#'
#' @examples
calc_col_ks <- function(result_files) {

  for (i in seq_along(result_files)) {
    results <- readRDS(result_files[i])

    ideal_col_times_list <- list()
    for (j in seq_along(results$island$ideal_islands)) {
      col_times <- c()
      for (k in 2:length(results$island$ideal_islands[[j]])) {
        stac <- results$island$ideal_islands[[j]][[k]]$stac
        if (stac == 1 || stac == 2 || stac == 4 || stac == 5 || stac == 6) {
          col_times <- c(col_times,
                         results$island$ideal_islands[[j]][[k]]$branching_times[2])
        } else {
          for (l in seq_along(results$island$ideal_islands[[j]][[k]]$all_colonisations)) {
            col_times <- c(col_times,
                           results$island$ideal_islands[[j]][[k]]$all_colonisations[[l]]$event_times[[2]])
          }
        }
      }
      ideal_col_times_list[[j]] <- col_times
    }
    ideal_col_times <- sort(unlist(ideal_col_times_list), decreasing = TRUE)

    empirical_col_times_list <- list()
    for (j in seq_along(results$island$empirical_islands)) {
      col_times <- c()
      for (k in 2:length(results$island$empirical_islands[[j]])) {
        stac <- results$island$empirical_islands[[j]][[k]]$stac
        if (stac == 1 || stac == 2 || stac == 4 || stac == 5 || stac == 6) {
          col_times <- c(col_times,
                         results$island$empirical_islands[[j]][[k]]$branching_times[2])
        } else {
          for (l in seq_along(results$island$ideal_islands[[j]][[k]]$all_colonisations)) {
            col_times <- c(col_times,
                           results$island$ideal_islands[[j]][[k]]$all_colonisations[[l]]$event_times[[2]])
          }
        }
      }
      empirical_col_times_list[[j]] <- col_times
    }
    empirical_col_times <- sort(unlist(empirical_col_times_list), decreasing = TRUE)
  }
  return(col_times_list)
}
