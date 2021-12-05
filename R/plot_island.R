#' Plot the simulated island, based on ideal and empirical data.
#'
#' Plot the simulated island, based on ideal and empirical data.
#'
#' The plot will consist of a grid,
#' with two columns and one or more rows.
#' The columns depict the same evolutionary history of the
#' island, yet for different type of data.
#' The rows depict the species clades.
#'
#' The vertical lines in the column for the ideal data depict
#' one or more colonisation events.
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
#' set.seed(
#'   9,
#'   kind = "Mersenne-Twister",
#'   normal.kind = "Inversion",
#'   sample.kind = "Rejection"
#' )
#' mainland <- sim_mainland(
#'   total_time = 10,
#'   m = 10,
#'   mainland_ex = 1.0
#' )
#' mainland_clade <- mainland[[1]]
#' plot_mainland_clade(mainland_clade)
#' island <- sim_island(
#'   total_time = 1,
#'   island_pars = c(1, 1, 10, 12, 1),
#'   mainland = mainland_clade,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete")
#' plot_island(island)
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
plot_island <- function(island) {
  # Fix build warnings
  branching_times <- NULL; rm(branching_times) # nolint, fixes warning: no visible binding for global variable
  unique_species_id <- NULL; rm(unique_species_id) # nolint, fixes warning: no visible binding for global variable
  stac_str <- NULL; rm(stac_str) # nolint, fixes warning: no visible binding for global variable

  t <- DAISIEmainland::island_to_tables(island)

  # Convert species_type to species_type_str
  t$colonisations$species_type_str <- as.character(Vectorize(
      DAISIEmainland::species_type_to_str)(t$colonisations$species_type))


  t$colonisations$species_type_str <- as.factor(t$colonisations$species_type_str)


  # Draw lines, with time going from past/left to present/right
  # x1 = x = branching_times                                                    # nolint this is no commented code
  # NO IDEA YET x2 = xend = spec_ex_t                                           # nolint this is no commented code
  # y1 = y = unique_species_id                                                  # nolint this is no commented code
  # y2 = yend = unique_species_id                                               # nolint this is no commented code
  # color = unique_species_id                                                   # nolint this is no commented code
  ggplot2::ggplot(data = t$speciations) +
    ggplot2::geom_vline(
      data = t$colonisations,
      ggplot2::aes(xintercept = event_times, linetype = species_type_str)
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = branching_times,
        y = unique_species_id,
        color = unique_species_id,
        shape = stac_str
      ),
      size = 2
    ) +
    ggplot2::facet_grid(
      clade_id ~ data_type,
      scales = "free",
      space = "free"
    )
}
