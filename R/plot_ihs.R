#' Plots the true value versus the inverse hyperbolic sine tranformed value to
#' show the relationship.
#'
#' @inheritParams default_params_doc
#'
#' @return Void (saves plot)
plot_ihs <- function(output_file_path) {
  true_data <- seq(-100000, 100000, 5)
  transformed_data <- asinh(true_data)
  plotting_data <- data.frame(true = true_data,
                              transformed = transformed_data)
  ihs_plot <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = true_data,
                                              y = transformed_data),
                       lwd = 2) +
    ggplot2::theme_classic() +
    ggplot2::ylab("Transformed value") +
    ggplot2::xlab("True value")
  if (!is.null(output_file_path)) {
    ggplot2::ggsave(
      plot = ihs_plot,
      filename = output_file_path,
      device = "png",
      width = 168,
      height = 100,
      units = "mm",
      dpi = 600
    )
  } else {
    return(ihs_plot)
  }
}

