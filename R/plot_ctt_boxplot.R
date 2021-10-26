#' Plots a heatmap of the colonisation through time statistic for mainland
#' sampling probability (y-axis) and mainland extinction rate (x-axis).
#'
#' @inheritParams default_params_doc
#'
#' @return Void (saves plot)
#' @export
plot_ctt_boxplot <- function(data_folder_path,
                             output_file_path,
                             parameter) {

  testit::assert(
    "Parameter must be either 'mainland_ex', 'unsampled', 'undiscovered'",
    parameter == "mainland_ex" || parameter == "unsampled" ||
      parameter == "undiscovered")

  files <- list.files(data_folder_path)

  if (length(files) == 0) {
    stop("No results are in the results directory")
  } else {
    file_paths <- as.list(paste0(data_folder_path, "/", files))
    results_list <- lapply(file_paths, readRDS)
  }

  error_list <- lapply(results_list, "[[", "error")
  sim_params_list <- lapply(results_list, "[[", "sim_params")

  ctt_list <- lapply(error_list, "[[", "delta_ctt")
  upper_ylim <- max(unlist(ctt_list)) + 0.05

  mainland_ex <- unlist(lapply(sim_params_list, "[[", "mainland_ex"))
  mainland_sample_prob <- unlist(lapply(sim_params_list, "[[",
                                        "mainland_sample_prob"))
  mainland_sample_type <- unlist(lapply(sim_params_list, "[[",
                                        "mainland_sample_type"))

  mainland_ex_list <- list()
  mainland_sample_prob_list <- list()
  mainland_sample_type_list <- list()
  for (i in seq_along(ctt_list)) {
    mainland_ex_list[[i]] <- rep(mainland_ex[i], length(ctt_list[[i]]))
    mainland_sample_prob_list[[i]] <- rep(mainland_sample_prob[i],
                                          length(ctt_list[[i]]))
    mainland_sample_type_list[[i]] <- rep(mainland_sample_type[i],
                                          length(ctt_list[[i]]))
  }

  plotting_data <- data.frame(
    ctt = unlist(ctt_list),
    mainland_ex = unlist(mainland_ex_list),
    mainland_sample_prob = unlist(mainland_sample_prob_list),
    mainland_sample_type = unlist(mainland_sample_type_list))

  if (parameter == "mainland_ex") {
    plotting_data <- dplyr::filter(
      plotting_data,
      plotting_data$mainland_sample_type == "complete")
  } else if (parameter == "unsampled") {
    plotting_data <- dplyr::filter(
      plotting_data,
      plotting_data$mainland_sample_type == "unsampled")
  } else if (parameter == "undiscovered") {
    plotting_data <- dplyr::filter(
      plotting_data,
      plotting_data$mainland_sample_type == "undiscovered")
  }

  if (parameter == "mainland_ex") {
    ctt <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(mainland_ex),
                                         y = ctt),
                            fill = "#56B4E9",
                            outlier.size = 0.5,
                            lwd = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression(paste(Delta, "CTT"))) +
      ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M])))) +
      ggplot2::ylim(c(0, upper_ylim))
  } else {
    ctt <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(mainland_sample_prob),
                                       y = ctt),
                          fill = "#56B4E9",
                          outlier.size = 0.5,
                          lwd = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression(paste(Delta, "CTT"))) +
      ggplot2::xlab(expression(paste("Mainland sample probability ", (rho)))) +
      ggplot2::ylim(c(0, upper_ylim))
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
