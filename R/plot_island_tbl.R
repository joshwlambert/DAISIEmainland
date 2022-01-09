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
#' island_tbl <- DAISIEmainland:::sim_island(
#'   total_time = 1,
#'   island_pars = c(1, 1, 10, 12, 1),
#'   mainland = mainland_clade,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete")
#' plot_island_tbl(island_tbl)
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
plot_island_tbl <- function(total_time,
                            island_tbl) {

  DAISIEmainland::check_island_tbl(island_tbl)

  # Fix build warnings
  col_t <- NULL; rm(col_t) # nolint, fixes warning: no visible binding for global variable
  spec_id <- NULL; rm(spec_id) # nolint, fixes warning: no visible binding for global variable
  species_type_str <- NULL; rm(species_type_str) # nolint, fixes warning: no visible binding for global variable




  # Convert species_type to factor species_type_str
  island_tbl$species_type_str <- as.character(Vectorize(
      DAISIEmainland::species_type_to_str)(island_tbl$spec_type))
  island_tbl$species_type_str <- as.factor(island_tbl$species_type_str)

  # Draw lines, with time going from past/left to present/right
  # x1 = x = col_t                                                              # nolint this is no commented code
  # x2 = till the end, unless cladgenetic, then until ...?                                                       # nolint this is no commented code
  # y1 = spec_id                                                  # nolint this is no commented code
  # y2 = spec_id                                               # nolint this is no commented code
  # color = unique_species_id                                                   # nolint this is no commented code
  ggplot2::ggplot(data = island_tbl) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = 0), lty = 2
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = col_t,
        y = spec_id,
        xend = total_time,
        yend = spec_id,
        col = species_type_str)
      ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = col_t,
        y = spec_id
      ),
      size = 3
    )
  # +
  #   ggplot2::facet_grid(
  #     clade_id ~ data_type,
  #     scales = "free",
  #     space = "free"
  #   )
}
