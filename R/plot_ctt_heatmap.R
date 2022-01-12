#' Plots a heatmap of the colonisation through time statistic for mainland
#' sampling probability (y-axis) and mainland extinction rate (x-axis).
#'
#' @inheritParams default_params_doc
#'
#' @return Void (saves plot)
#' @export
plot_ctt_heatmap <- function(analysis_results,
                             output_file_path) {
  error_list <- lapply(analysis_results, "[[", "error")
  sim_params_list <- lapply(analysis_results, "[[", "sim_params")

  ctt_list <- lapply(error_list, "[[", "delta_ctt")
  ctt_means <- unlist(lapply(ctt_list, mean))

  mainland_ex <- unlist(lapply(sim_params_list, "[[", 6))
  mainland_sample_prob <- unlist(lapply(sim_params_list, "[[", 7))

  plotting_data <- data.frame(
    ctt_means = ctt_means,
    mainland_ex = mainland_ex,
    mainland_sample_prob = mainland_sample_prob
  )

  ctt <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_tile(ggplot2::aes(
      x = mainland_ex,
      y = mainland_sample_prob,
      fill = ctt_means
    )) +
    ggplot2::theme_classic() +
    ggplot2::ylab(expression(paste("Mainland sampling probability ", (rho)))) +
    ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M])))) +
    ggplot2::scale_fill_continuous(name = expression(paste(Delta, "nCTT")))

  if (!is.null(output_file_path)) {
    ggplot2::ggsave(
      plot = ctt,
      filename = output_file_path,
      device = "png",
      width = 180,
      height = 180,
      units = "mm",
      dpi = 600
    )
  } else {
    return(ctt)
  }
}
