#' Plot the simulated mainland.
#' @inheritParams default_params_doc
#'
#' @return a `ggplot2`
#'
#' @examples
#' mainland_clade <- DAISIEmainland:::create_test_mainland_clade(
#'   mainland_scenario = 2)
#' plot_mainland_clade(mainland_clade)
#' @author Richèl J.C. Bilderbeek
#' @export
plot_mainland_clade <- function(mainland_clade) {
  mainland <- list()
  mainland[[1]] <- mainland_clade
  DAISIEmainland::plot_mainland(mainland = mainland)
}
