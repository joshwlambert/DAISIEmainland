#' Plots a heatmap of the colonisation through time statistic for mainland
#' sampling probability (y-axis) and mainland extinction rate (x-axis).
#'
#' @inheritParams default_params_doc
#'
#' @return Void (saves plot)
#' @export
plot_ctt_scatter <- function(data_folder_path,
                             output_file_path,
                             param_space) {

  testit::assert(
    "Param_space must be either 'mainland_ex' or 'mainland_sample_prob'",
    param_space == "mainland_ex" || param_space == "mainland_sample_prob")

  files <- list.files(data_folder_path, pattern = param_space)

  if (length(files) == 0) {
    stop("No results are in the results directory")
  } else {
    file_paths <- as.list(paste0(data_folder_path, "/", files))
    results_list <- lapply(file_paths, readRDS)
  }

  error_list <- lapply(results_list, "[[", "error")
  sim_params_list <- lapply(results_list, "[[", "sim_params")

  ctt_list <- lapply(error_list, "[[", "delta_ctt")
  ctt_means <- unlist(lapply(ctt_list, mean))

  mainland_ex <- unlist(lapply(sim_params_list, "[[", 6))
  mainland_sample_prob <- unlist(lapply(sim_params_list, "[[", 7))

  plotting_data <- data.frame(ctt_means = ctt_means,
                              mainland_ex = mainland_ex,
                              mainland_sample_prob = mainland_sample_prob)

  if (param_space == "mainland_ex") {
    ctt <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_point(ggplot2::aes(x = mainland_ex, y = ctt_means)) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression(paste(Delta, "CTT"))) +
      ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M]))))
  } else {
    ctt <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_point(ggplot2::aes(x = mainland_sample_prob,
                                       y = ctt_means)) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression(paste(Delta, "CTT"))) +
      ggplot2::xlab(expression(paste("Mainland sample probability ", (rho))))
  }

  if (!is.null(output_file_path)) {
    ggplot2::ggsave(
      plot = ctt,
      filename = output_file_path,
      device = "png",
      width = 168,
      height = 100,
      units = "mm",
      dpi = 600
    )
  } else {
    return(ctt)
  }
}
