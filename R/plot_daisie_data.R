#' Plot the DAISIE data
#'
#' Plot the DAISIE data
#'
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
#' total_time <- 1
#' m <- 10 # number of mainland clades
#' mainland <- sim_mainland(
#'   total_time = total_time,
#'   m = m,
#'   mainland_ex = 2.0
#' )
#' mainland_clade <- mainland[[1]]
#' plot_mainland_clade(mainland_clade)
#' island <- DAISIEmainland:::sim_island(
#'   total_time = total_time,
#'   island_pars = c(1, 1, 10, 12, 1),
#'   mainland = mainland_clade,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete")
#' plot_island(island)
#' daisie_data <- DAISIEmainland:::format_to_daisie_data(
#'   island_replicates = island,
#'   total_time = total_time,
#'   m = m
#' )
#' plot_daisie_data(daisie_data)
#' @author Richèl J.C. Bilderbeek
#'
#' @export
plot_daisie_data <- function(daisie_data) {
  DAISIEmainland::check_daisie_data(daisie_data)

  # There appears to be no difference
  # Fix build warnings
  branching_times <- NULL; rm(branching_times) # nolint, fixes warning: no visible binding for global variable
  unique_species_id <- NULL; rm(unique_species_id) # nolint, fixes warning: no visible binding for global variable
  stac_str <- NULL; rm(stac_str) # nolint, fixes warning: no visible binding for global variable
  event_times <- NULL; rm(event_times) # nolint, fixes warning: no visible binding for global variable
  species_type_str <- NULL; rm(species_type_str) # nolint, fixes warning: no visible binding for global variable

  t <- DAISIEmainland::daisie_data_to_tables(daisie_data)

  # Convert species_type to factor species_type_str
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
      size = 3
    ) +
    ggplot2::facet_grid(
      clade_id ~ data_type,
      scales = "free",
      space = "free"
    )
}
