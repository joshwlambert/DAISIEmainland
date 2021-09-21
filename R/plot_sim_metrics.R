#' Plots metrics from simulations
#'
#' @param sim_metrics A list of metrics output from [calc_sim_metrics()]
#'
#' @return none Four plots are shown: histogram of the number of species,
#' histogram of the number of colonizations, histogram of the largest clade size
#' and histogram of the rank of the largest clade
#' @author Joshua W. Lambert
#' @export plot_sim_metrics
plot_sim_metrics <- function(sim_metrics) {

  plotting_data <- data.frame(
    num_spec = sim_metrics$num_spec,
    num_col = sim_metrics$num_colonists,
    max_clade_size = sim_metrics$max_clade_size)

  num_spec <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = number_spec),
                            fill = "cornsilk3") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = median(number_spec))) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Number of Species") +
    ggplot2::ylab("Frequency")

  num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = number_colonists),
                            fill = "cornsilk3") +
    ggplot2::geom_vline(ggplot2::aes(
      xintercept = stats::median(number_colonists))) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Number of Colonisation events") +
    ggplot2::ylab("Frequency")

  size_largest_clade <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = size_largest_clade),
                            fill = "cornsilk3") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = median(size_largest_clade))) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Size of largest clade") +
    ggplot2::ylab("Frequency")

  cowplot::plot_grid(num_spec, num_col, size_largest_clade, nrow = 1)
}
