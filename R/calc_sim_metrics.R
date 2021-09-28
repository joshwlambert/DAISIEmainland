#' Calculates summary metrics from a simulation
#'
#' @inheritParams default_params_doc
#'
#' @return List of simulation metrics
#' @export
#' @author Joshua W. Lambert
calc_sim_metrics <- function(daisie_data) {

  num_col <- c()
  num_spec <- c()

  for (i in seq_along(daisie_data)) {
    sim_rep <- daisie_data[[i]]

    temp_num_spec <- c()
    temp_num_col <- c()

    stacs <- lapply(sim_rep, "[[", "stac")

    for (j in 2:length(sim_rep)) {
      if (stacs[[j]] != 3) {
        temp_num_spec <- c(
          temp_num_spec,
          length(sim_rep[[j]]$branching_times) - 1
        )
        temp_num_col <- c(
          temp_num_col,
          1
        )
      } else {
        for (k in seq_along(sim_rep[[j]]$all_colonisations)) {
          temp_num_spec <- c(
            temp_num_spec,
            length(sim_rep[[j]]$all_colonisations[[k]]$event_times) - 1
          )
          temp_num_col <- c(
            temp_num_col,
            1
          )
        }
      }
    }
    num_spec <- c(num_spec, sum(temp_num_spec))
    num_col <- c(num_col, sum(temp_num_col))
  }

  sim_metrics <- list(num_col = num_col,
                      num_spec = num_spec)

  return(sim_metrics)
}
