#' Plots a heatmap of the colonisation through time statistic for mainland
#' sampling probability (y-axis) and mainland extinction rate (x-axis).
#'
#' @inheritParams default_params_doc
#'
#' @return Void (saves plot)
#' @export
plot_ctt <- function(output_filename = file.path(here::here(),
                                                 "plots",
                                                 "ctt.png"),
                     save = TRUE,
                     test = FALSE) {

  if (test) {
    file_path <- file.path(here::here(), "tests", "testthat", "testdata")
  } else {
    file_path <- file.path(here::here(), "results")
  }

  files <- list.files(file_path)

  if (length(files) == 0) {
    stop("No results are in the results directory")
  } else {
    file_paths <- as.list(paste0(file_path, "/", files))
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

  ctt <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_tile(ggplot2::aes(x = mainland_ex,
                                    y = mainland_sample_prob,
                                    fill = ctt_means)) +
    ggplot2::theme_classic() +
    ggplot2::ylab(expression(paste("Mainland sampling probability ", (rho)))) +
    ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M])))) +
    ggplot2::scale_fill_continuous(name = expression(paste(Delta, "CTT"))) +
    ggplot2::scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5))

  if (save) {
      ggplot2::ggsave(
        plot = ctt,
        filename = output_filename,
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
