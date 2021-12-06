#' Plot the simulated mainland.
#' @inheritParams default_params_doc
#'
#' @return a `ggplot2`
#'
#' @seealso
#' These are the functions to plot an evolutionary history:
#'
#'  * Use \link{plot_mainland} to plot the mainland history
#'  * Use \link{plot_island} to plot the island history.
#'    based on both empirical data and ideal data.
#'  * Use \link{plot_empirical_island} to plot the island history.
#'    based on empirical data.
#'  * Use \link{plot_ideal_island} to plot the island history
#'    based on ideal data.
#'
#' @examples
#' mainland <- DAISIEmainland::sim_mainland(
#'   total_time = 1,
#'   m = 2,
#'   mainland_ex = 2)
#'
#' plot_mainland(mainland)
#'
#' @author Richèl J.C. Bilderbeek
#' @export
plot_mainland <- function(mainland) {

  # Fix build warnings
  spec_origin_t <- NULL; rm(spec_origin_t) # nolint, fixes warning: no visible binding for global variable
  spec_ex_t <- NULL; rm(spec_ex_t) # nolint, fixes warning: no visible binding for global variable
  unique_species_id <- NULL; rm(unique_species_id) # nolint, fixes warning: no visible binding for global variable

  # Give each list element a clade id
  for (i in seq_along(mainland)) {
    mainland[[i]]$clade_id <- i
  }
  # Combine the list into one big tibble
  t_mainland <- dplyr::bind_rows(mainland)

  # Number all species of all clades indivually
  t_mainland$unique_species_id <- seq(1, nrow(t_mainland))
  t_mainland$unique_species_id <- as.factor(t_mainland$unique_species_id)

  # Draw lines, with time going from past/left to present/right
  # x1 = x = = spec_origin_t
  # x2 = xend = spec_ex_t (always conveniently stopped at time 'total_time')
  # y1 = y = unique_species_id                                                  # nolint this is no commented code
  # y2 = yend = unique_species_id                                               # nolint this is no commented code
  # color = unique_species_id                                                   # nolint this is no commented code
  ggplot2::ggplot(data = t_mainland) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = spec_origin_t,
        xend = spec_ex_t,
        y = unique_species_id,
        yend = unique_species_id,
        color = unique_species_id
      )
    ) +
    ggplot2::facet_grid(
      clade_id ~ .,
      scales = "free",
      space = "free"
    )

}
