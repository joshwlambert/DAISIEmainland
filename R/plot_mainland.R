#' Plot the simulated mainland.
#'
#' @inheritParams default_params_doc
#'
#' @return a `ggplot2` facet plot, where each facet/row contains
#' the evolutionary history of a clade.
#' Per clade, the x axis shows the time from past (at the left)
#' to the present (at the right).
#' The y-axis has no meaning except for separating the different
#' species using \link{branch_code_to_y}.
#'
#' @seealso
#' These are the functions to plot an evolutionary history:
#'
#'  * Use \link{plot_mainland} to plot the mainland history
#'
#' @examples
#' mainland <- DAISIEmainland::sim_mainland(
#'   total_time = 1,
#'   m = 2,
#'   mainland_ex = 2
#' )
#'
#' plot_mainland(mainland)
#' @author Richèl J.C. Bilderbeek
#' @export
plot_mainland <- function(mainland) {

  # Fix build warnings
  spec_origin_t <- NULL; rm(spec_origin_t) # nolint, fixes warning: no visible binding for global variable
  spec_ex_t <- NULL; rm(spec_ex_t) # nolint, fixes warning: no visible binding for global variable
  unique_species_id <- NULL; rm(unique_species_id) # nolint, fixes warning: no visible binding for global variable
  y <- NULL; rm(y) # nolint, fixes warning: no visible binding for global variable
  ancestor_spec_ex_t <- NULL; rm(ancestor_spec_ex_t) # nolint, fixes warning: no visible binding for global variable
  ancestor_y <- NULL; rm(ancestor_y) # nolint, fixes warning: no visible binding for global variable
  offspring_spec_origin_t <- NULL; rm(offspring_spec_origin_t) # nolint, fixes warning: no visible binding for global variable
  offspring_y <- NULL; rm(offspring_y) # nolint, fixes warning: no visible binding for global variable

  # Give each list element a clade id
  for (i in seq_along(mainland)) {
    mainland[[i]]$clade_id <- i
  }

  # Combine the list into one big tibble
  # x1 = x = = spec_origin_t
  # x2 = xend = spec_ex_t (always conveniently stopped at time 'total_time')
  # y1 = y = unique_species_id                                                  # nolint this is no commented code
  # y2 = yend = unique_species_id                                               # nolint this is no commented code
  # color = unique_species_id                                                   # nolint this is no commented code
  t_mainland <- dplyr::bind_rows(mainland)
  t_mainland$y <- Vectorize(DAISIEmainland::branch_code_to_y)(
    t_mainland$branch_code
  )

  # Number all species of all clades individually
  # Keep the clade ID first; it is assumed the branch code is at the end:
  # by removing the last character, the ancestor is found
  t_mainland$unique_species_id <- paste0(
    t_mainland$clade_id, "-", t_mainland$branch_code
  )

  # Do not make a factor, as we need to work on the string
  # t_mainland$unique_species_id <- as.factor(t_mainland$unique_species_id)     # nolint yup, this is code

  # Create a table for the vertical lines
  # x1 = spec_ex_t (of ancestor)
  # x2 = spec_origin_t (of derived)
  # y1 = y of branch_code ancestor
  # y2 = y of branch_code of derived species
  #
  # Work backwards
  t_ancestors <- data.frame(
    ancestor_branch_code = t_mainland$branch_code,
    ancestor_unique_species_id = t_mainland$unique_species_id,
    ancestor_spec_ex_t = t_mainland$spec_ex_t,
    clade_id = t_mainland$clade_id
  )
  t_offspring <- data.frame(
    offspring_branch_code = t_mainland$branch_code,
    offspring_unique_species_id = t_mainland$unique_species_id,
    ancestor_unique_species_id = strtrim(
      t_mainland$unique_species_id,
      nchar(t_mainland$unique_species_id) - 1
    ),
    offspring_spec_origin_t = t_mainland$spec_origin_t
  )
  t_vertical <- merge(t_ancestors, t_offspring)
  t_vertical$ancestor_y <- Vectorize(DAISIEmainland::branch_code_to_y)(
    t_vertical$ancestor_branch_code
  )
  t_vertical$offspring_y <- Vectorize(DAISIEmainland::branch_code_to_y)(
    t_vertical$offspring_branch_code
  )

  # Here, we reverse the time axis,
  # from time after the island came into existance,
  # to time before present

  # aka the island age. There is always a species at the present
  total_time <- max(t_mainland$spec_ex_t)
  t_mainland$spec_origin_t <- total_time - t_mainland$spec_origin_t
  t_mainland$spec_ex_t <- total_time - t_mainland$spec_ex_t
  t_vertical$ancestor_spec_ex_t <- total_time - t_vertical$ancestor_spec_ex_t
  t_vertical$offspring_spec_origin_t <- total_time - t_vertical$offspring_spec_origin_t # nolint indeed a long line

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = t_mainland,
      ggplot2::aes(
        x = spec_origin_t,
        xend = spec_ex_t,
        y = y,
        yend = y,
        color = unique_species_id
      )
    ) +
    ggplot2::geom_segment(
      data = t_vertical,
      ggplot2::aes(
        x = ancestor_spec_ex_t,
        xend = offspring_spec_origin_t,
        y = ancestor_y,
        yend = offspring_y
      )
    ) + ggplot2::scale_x_reverse(
      name = "Time before present",
      limits = c(total_time, 0)
    ) +
    ggplot2::facet_grid(
      clade_id ~ .,
      scales = "free",
      space = "free"
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
}
