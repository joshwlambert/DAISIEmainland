#' Calculates summary metrics from a simulation
#'
#' @inheritParams default_params_doc
#'
#' @return Numeric vector number of species for each island replicate
#' @export
#' @author Joshua W. Lambert
calc_num_spec <- function(multi_daisie_data) {
  num_spec <- c()
  for (i in seq_along(multi_daisie_data)) {
    daisie_data <- multi_daisie_data[[i]]
    temp_num_spec <- c()
    stacs <- lapply(daisie_data, "[[", "stac")
    for (j in 2:length(daisie_data)) {
      if (stacs[[j]] != 3) {
        temp_num_spec <- c(
          temp_num_spec,
          length(daisie_data[[j]]$branching_times) - 1
        )
      } else {
        for (k in seq_along(daisie_data[[j]]$all_colonisations)) {
          temp_num_spec <- c(
            temp_num_spec,
            length(daisie_data[[j]]$all_colonisations[[k]]$event_times) - 1
          )
        }
      }
    }
    num_spec <- c(num_spec, sum(temp_num_spec))
  }
  return(num_spec)
}
