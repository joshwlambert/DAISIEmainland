plot_ctt <- function() {

  files <- list.files(file.path(getwd(), "results"))

  if (length(files) == 0) {
    stop("No results are in the results directory")
  } else {
    file_paths <- as.list(paste0(file.path(getwd(), "results"), "/", files))
    results_list <- lapply(file_paths, readRDS)
  }

  error_list <- lapply(results_list, '[[', "error")
  sim_params_list <- lapply(results_list, '[[', "sim_params")

  ctt_list <- lapply(error_list, '[[', "delta_ctt")
  ctt_means <- unlist(lapply(ctt_list, mean))

  mainland_ex <- unlist(lapply(sim_params_list, '[[', 6))
  mainland_sample_prob <- unlist(lapply(sim_params_list, '[[', 7))

  plotting_data <- data.frame(ctt_means = ctt_means,
                              mainland_ex = mainland_ex,
                              mainland_sample_prob = mainland_sample_prob)

  ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_tile(mapping = ggplot2::aes(x = mainland_ex,
                                              y = mainland_sample_prob)) +
    ggplot2::theme_classic()
}
