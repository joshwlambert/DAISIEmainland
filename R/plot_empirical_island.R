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
#' plot_empirical_island(island$empirical_island)
#' @author Richèl J.C. Bilderbeek
#' @export
plot_empirical_island <- function(empirical_island) {
  # Can use the same island
  # TODO: Maybe let 'plot_empirical_island' and 'plot_empirical_island'
  # call 'plot_island'
  DAISIEmainland::plot_ideal_island(ideal_island = empirical_island)
}
