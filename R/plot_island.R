#' Plot the simulated island, based on ideal and empirical data.
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
#'   mainland_scenario = mainland_scenario
#' )
#' island <- DAISIEmainland:::sim_island(
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
  t <- DAISIEmainland::island_to_tables(island)

  # Draw lines, with time going from past/left to present/right
  # x1 = x = branching_times
  # NO IDEA YET x2 = xend = spec_ex_t
  # y1 = y = unique_species_id
  # y2 = yend = unique_species_id
  # color = unique_species_id
  ggplot2::ggplot(data = t$t) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = branching_times,
        y = unique_species_id,
        color = unique_species_id,
        shape = stac_str
      )
    ) +
    ggplot2::facet_grid(
      clade_id ~ data_type,
      scales = "free",
      space = "free"
    )
}
