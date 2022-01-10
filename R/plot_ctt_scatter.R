#' Plots points of the mean delta colonisation through time statistic (y-axis)
#' for either mainland extinction rate or mainland sampling probability
#' (x-axis).
#'
#' @inheritParams default_params_doc
#'
#' @return Void (saves plot)
#' @export
plot_ctt_scatter <- function(analysis_results,
                             output_file_path,
                             parameter) {

  testit::assert(
    "Parameter must be either 'mainland_ex', 'unsampled', 'undiscovered'",
    parameter == "mainland_ex" || parameter == "unsampled" ||
      parameter == "undiscovered")

  error_list <- lapply(analysis_results, "[[", "error")
  sim_params_list <- lapply(analysis_results, "[[", "sim_params")

  ctt_list <- lapply(error_list, "[[", "delta_ctt")
  ctt_means <- unlist(lapply(ctt_list, mean))

  mainland_ex <- unlist(lapply(sim_params_list, "[[", "mainland_ex"))
  mainland_sample_prob <- unlist(lapply(sim_params_list, "[[",
                                        "mainland_sample_prob"))
  mainland_sample_type <- unlist(lapply(sim_params_list, "[[",
                                        "mainland_sample_type"))

  plotting_data <- data.frame(ctt_means = ctt_means,
                              mainland_ex = mainland_ex,
                              mainland_sample_prob = mainland_sample_prob,
                              mainland_sample_type = mainland_sample_type)

  if (parameter == "mainland_ex") {
    plotting_data <-
      plotting_data[plotting_data$mainland_sample_type %in% "complete", ]
  } else {
    plotting_data <-
      plotting_data[plotting_data$mainland_sample_type %in% parameter, ]
  }

  if (parameter == "mainland_ex") {
    ctt <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_point(ggplot2::aes(x = mainland_ex,
                                       y = ctt_means),
                          colour = "#56B4E9",
                          size = 2) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression(paste(Delta, "nCTT"))) +
      ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M]))))
  } else {
    ctt <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_point(ggplot2::aes(x = mainland_sample_prob,
                                       y = ctt_means),
                          colour = "#56B4E9",
                          size = 2) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression(paste(Delta, "nCTT"))) +
      ggplot2::xlab(expression(paste("Mainland sample probability ", (rho))))
  }

  if (!is.null(output_file_path)) {
    ggplot2::ggsave(
      plot = ctt,
      filename = output_file_path,
      device = "png",
      width = 80,
      height = 80,
      units = "mm",
      dpi = 600
    )
  } else {
    return(ctt)
  }
}
