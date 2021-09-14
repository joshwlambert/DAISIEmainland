#' Title
#'
#' @return
#' @export
#'
#' @examples
plot_k_estimates <- function() {

  files <- list.files(file.path(getwd(), "results"))

  if (length(files) == 0) {
    stop("No results are in the results directory")
  } else {
    file_paths <- as.list(paste0(file.path(getwd(), "results"), "/", files))
    results_list <- lapply(file_paths, readRDS)
  }

  ideal_params_list <- lapply(results_list, '[[', "ideal_ml")
  empirical_params_list <- lapply(results_list, '[[', "empirical_ml")
  ideal_k_list <- lapply(ideal_params_list, function(x) {
    unlist(lapply(x, '[[', "K"))
  })
  empirical_k_list <- lapply(empirical_params_list, function(x) {
    unlist(lapply(x, '[[', "K"))
  })
  ideal_mean_k <- unlist(lapply(ideal_k_list, mean))
  empirical_mean_k <- unlist(lapply(empirical_k_list, mean))
  ideal_mean_k <- ideal_mean_k[!is.na(ideal_mean_k)] #remove this and fix NAs upstream
  empirical_mean_k <- empirical_mean_k[!is.na(empirical_mean_k)] #remove this and fix NAs upstream
  sim_params_list <- lapply(results_list, '[[', "sim_params")

  num_ideal_inf_k <- length(which(ideal_mean_k == Inf))
  num_empirical_inf_k <- length(which(empirical_mean_k == Inf))
  num_ideal_k <- length(ideal_mean_k)
  num_empirical_k <- length(empirical_mean_k)

  ideal_mean_k_no_inf <- ideal_mean_k[!is.infinite(ideal_mean_k)]
  empirical_mean_k_no_inf <- empirical_mean_k[!is.infinite(empirical_mean_k)]

  mainland_ex <- unlist(lapply(sim_params_list, '[[', 6))
  mainland_sample_prob <- unlist(lapply(sim_params_list, '[[', 7))

  plotting_data <- data.frame(ideal_mean_k_no_inf = ideal_mean_k_no_inf,
                              empirical_mean_k_no_inf = empirical_mean_k_no_inf,
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

  ggplot2::ggsave(
    plot = ctt,
    filename = file.path("plots", "ctt"),
    device = "png",
    width = 168,
    height = 100,
    units = "mm",
    dpi = 600
  )
}
