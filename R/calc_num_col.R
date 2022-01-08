#' Calculates summary metrics from a simulation
#'
#' @inheritParams default_params_doc
#'
#' @return Numeric vector number of colonisations for each island replicate
#' @export
#' @author Joshua W. Lambert
calc_num_col <- function(multi_daisie_data) {
  num_col <- c()
  for (i in seq_along(multi_daisie_data)) {
    daisie_data <- multi_daisie_data[[i]]
    temp_num_col <- c()
    stacs <- lapply(daisie_data, "[[", "stac")
    for (j in 2:length(daisie_data)) {
      if (stacs[[j]] != 3) {
        temp_num_col <- c(
          temp_num_col,
          1
        )
      } else {
        for (k in seq_along(daisie_data[[j]]$all_colonisations)) {
          temp_num_col <- c(
            temp_num_col,
            1
          )
        }
      }
    }
    num_col <- c(num_col, sum(temp_num_col))
  }
  return(num_col)
}
