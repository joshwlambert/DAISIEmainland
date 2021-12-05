#' Plot the simulated ideal island.
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
#' mainland_clade <- DAISIEmainland:::create_test_mainland_clade(
#'   mainland_scenario = 2)
#' island <- DAISIEmainland:::sim_island(
#'   total_time = 1,
#'   island_pars = c(1, 1, 10, 1, 1),
#'   mainland = mainland_clade,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete")
#' plot_ideal_island(island$ideal_island)
#'
#' @author Richèl J.C. Bilderbeek
#' @export
plot_ideal_island <- function(ideal_island) {

  # Fix build warnings
  branching_times <- NULL; rm(branching_times) # nolint, fixes warning: no visible binding for global variable
  unique_species_id <- NULL; rm(unique_species_id) # nolint, fixes warning: no visible binding for global variable
  stac_str <- NULL; rm(stac_str) # nolint, fixes warning: no visible binding for global variable

  # Move 'ideal_island$all_colonisations' to a seperate list
  all_colonisations <- list()
  for (i in seq_along(ideal_island)) {
    if (is.null(ideal_island[[i]]$all_colonisations)) next
    all_colonisations[[i]] <- ideal_island[[i]]$all_colonisations
    all_colonisations[[i]]$clade_id <- i
    ideal_island[[i]]$all_colonisations <- NULL
  }
  if (1 == 2) {
    # This is too complex for now
    dplyr::bind_rows(all_colonisations)
  }

  # Give each list element a clade id
  for (i in seq_along(ideal_island)) {
    ideal_island[[i]]$clade_id <- i
  }
  # Combine the list into one big tibble
  t_ideal_island <- dplyr::bind_rows(ideal_island)
  t_ideal_island$stac_str <- Vectorize(stac_to_str)(t_ideal_island$stac)
  t_ideal_island$stac_str <- as.factor(t_ideal_island$stac_str)

  # Number all species of all clades individually
  t_ideal_island$unique_species_id <- seq(1, nrow(t_ideal_island))
  t_ideal_island$unique_species_id <- as.factor(
    t_ideal_island$unique_species_id
  )

  # Draw lines, with time going from past/left to present/right
  # x1 = x = branching_times             # nolint this is no commented code
  # NO IDEA YET x2 = xend = spec_ex_t    # nolint this is no commented code
  # y1 = y = unique_species_id           # nolint this is no commented code
  # y2 = yend = unique_species_id        # nolint this is no commented code
  # color = unique_species_id            # nolint this is no commented code
  ggplot2::ggplot(data = t_ideal_island) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = branching_times,
        y = unique_species_id,
        color = unique_species_id,
        shape = stac_str
      )
    ) +
    ggplot2::facet_grid(
      clade_id ~ .,
      scales = "free",
      space = "free"
    )
}
