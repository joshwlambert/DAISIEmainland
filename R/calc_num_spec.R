#' Calculates summary metrics from a simulation
#'
#' @inheritParams default_params_doc
#'
#' @return Numeric vector number of species for each island replicate
#' @export
#' @author Joshua W. Lambert
calc_num_spec <- function(daisie_data) {
  num_spec <- c()
  for (i in seq_along(daisie_data)) {
    sim_rep <- daisie_data[[i]]
    temp_num_spec <- c()
    stacs <- lapply(sim_rep, "[[", "stac")
    for (j in 2:length(sim_rep)) {
      if (stacs[[j]] != 3) {
        temp_num_spec <- c(
          temp_num_spec,
          length(sim_rep[[j]]$branching_times) - 1
        )
      } else {
        for (k in seq_along(sim_rep[[j]]$all_colonisations)) {
          temp_num_spec <- c(
            temp_num_spec,
            length(sim_rep[[j]]$all_colonisations[[k]]$event_times) - 1
          )
        }
      }
    }
    num_spec <- c(num_spec, sum(temp_num_spec))
  }
  return(num_spec)
}
