#' Plot the simulated mainland.
#' @inheritParams default_params_doc
#'
#' @return a `ggplot2`
#'
#' @examples
#' mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
#' island <- sim_island(
#'   total_time = 1,
#'   island_pars = c(1, 1, 10, 1, 1),
#'   mainland = mainland_clade,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete")
#' if (1 == 2 ) {
#'   # TODO: add 'plot_ideal_island'
#'   plot_ideal_island(island$ideal_island)
#' }
#' @author Richèl J.C. Bilderbeek
#' @export
plot_ideal_island <- function(ideal_island) {


  # Fix build warnings
  branching_times <- NULL; rm(spec_origin_t) # nolint, fixes warning: no visible binding for global variable
  unique_species_id <- NULL; rm(unique_species_id) # nolint, fixes warning: no visible binding for global variable

  # Move 'ideal_island$all_colonisations' to a seperate list
  all_colonisations <- list()
  for (i in seq_along(ideal_island)) {
    all_colonisations[[i]] <- ideal_island[[i]]$all_colonisations
    ideal_island[[i]]$all_colonisations <- NULL
  }

  # Give each list element a clade id
  for (i in seq_along(ideal_island)) {
    ideal_island[[i]]$clade_id <- i
  }
  # Combine the list into one big tibble
  t_ideal_island <- dplyr::bind_rows(ideal_island)

  # Number all species of all clades individually
  t_ideal_island$unique_species_id <- seq(1, nrow(t_ideal_island))
  t_ideal_island$unique_species_id <- as.factor(t_ideal_island$unique_species_id)

  # Draw lines, with time going from past/left to present/right
  # x1 = x = branching_times
  # NO IDEA YET x2 = xend = spec_ex_t
  # y1 = y = unique_species_id
  # y2 = yend = unique_species_id
  # color = unique_species_id
  ggplot2::ggplot(data = t_ideal_island) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = branching_times,
        y = unique_species_id
      )
    ) +
    ggplot2::facet_grid(
      clade_id ~ .,
      scales = "free",
      space = "free"
    )
}
