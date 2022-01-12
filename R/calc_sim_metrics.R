#' Calculates the summary metrics for the entire parameter space
#'
#' @inheritParams default_params_doc
#'
#' @return List of simulation metrics
#' @export
#' @author Joshua W. Lambert
calc_sim_metrics <- function(analysis_results,
                             output_file_path) {
  ideal_sim_metrics_list <- lapply(analysis_results, "[[", "ideal_sim_metrics")
  empirical_sim_metrics_list <- lapply(
    analysis_results,
    "[[",
    "empirical_sim_metrics"
  )

  ideal_num_col_list <- lapply(
    ideal_sim_metrics_list,
    "[[",
    "ideal_sim_num_col"
  )
  ideal_num_spec_list <- lapply(
    ideal_sim_metrics_list,
    "[[",
    "ideal_sim_num_spec"
  )
  empirical_num_col_list <- lapply(
    empirical_sim_metrics_list,
    "[[",
    "empirical_sim_num_col"
  )
  empirical_num_spec_list <- lapply(
    empirical_sim_metrics_list,
    "[[",
    "empirical_sim_num_spec"
  )

  ideal_mean_num_col <- unlist(lapply(ideal_num_col_list, mean))
  ideal_max_num_col <- unlist(lapply(ideal_num_col_list, max))
  ideal_min_num_col <- unlist(lapply(ideal_num_col_list, min))

  ideal_mean_num_spec <- unlist(lapply(ideal_num_spec_list, mean))
  ideal_max_num_spec <- unlist(lapply(ideal_num_spec_list, max))
  ideal_min_num_spec <- unlist(lapply(ideal_num_spec_list, min))

  empirical_mean_num_col <- unlist(lapply(empirical_num_col_list, mean))
  empirical_max_num_col <- unlist(lapply(empirical_num_col_list, max))
  empirical_min_num_col <- unlist(lapply(empirical_num_col_list, min))

  empirical_mean_num_spec <- unlist(lapply(empirical_num_spec_list, mean))
  empirical_max_num_spec <- unlist(lapply(empirical_num_spec_list, max))
  empirical_min_num_spec <- unlist(lapply(empirical_num_spec_list, min))

  ideal_mean_num_col <- mean(ideal_mean_num_col)
  ideal_max_num_col <- max(ideal_max_num_col)
  ideal_min_num_col <- min(ideal_min_num_col)

  ideal_mean_num_spec <- mean(ideal_mean_num_spec)
  ideal_max_num_spec <- max(ideal_max_num_spec)
  ideal_min_num_spec <- min(ideal_min_num_spec)

  empirical_mean_num_col <- mean(empirical_mean_num_col)
  empirical_max_num_col <- max(empirical_max_num_col)
  empirical_min_num_col <- min(empirical_min_num_col)

  empirical_mean_num_spec <- mean(empirical_mean_num_spec)
  empirical_max_num_spec <- max(empirical_max_num_spec)
  empirical_min_num_spec <- min(empirical_min_num_spec)

  output <- list(
    ideal_mean_num_col = ideal_mean_num_col,
    ideal_max_num_col = ideal_max_num_col,
    ideal_min_num_col = ideal_min_num_col,
    ideal_mean_num_spec = ideal_mean_num_spec,
    ideal_max_num_spec = ideal_max_num_spec,
    ideal_min_num_spec = ideal_min_num_spec,
    empirical_mean_num_col = empirical_mean_num_col,
    empirical_max_num_col = empirical_max_num_col,
    empirical_min_num_col = empirical_min_num_col,
    empirical_mean_num_spec = empirical_mean_num_spec,
    empirical_max_num_spec = empirical_max_num_spec,
    empirical_min_num_spec = empirical_min_num_spec
  )

  if (!is.null(output_file_path)) {
    saveRDS(object = output, file = output_file_path)
  } else {
    return(output)
  }
}
