#' Plot the `daisie_data`.
#'
#' @inheritParams default_params_doc
#'
#' @return a `ggplot2`
#'
#' @author Richèl J.C. Bilderbeek
#' @export
plot_daisie_data <- function(daisie_data) {

  # Fix build warnings
  branching_times <- NULL; rm(branching_times) # nolint, fixes warning: no visible binding for global variable
  clade_index <- NULL; rm(clade_index) # nolint, fixes warning: no visible binding for global variable
  stac_str <- NULL; rm(stac_str) # nolint, fixes warning: no visible binding for global variable
  yend <- NULL; rm(yend) # nolint, fixes warning: no visible binding for global variable

  t <- DAISIEmainland::daisie_data_to_tables(daisie_data)

  p <- ggplot2::ggplot(t$colonists_general) +
    ggplot2::scale_x_continuous(
      name = "Time",
      limits = c(0, t$header$island_age)
    ) + ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  # Sometimes, no colonisations take place, making an island history dull ...
  if (nrow(t$colonists_general) == 0) {
    return(p)
  }

  # Remove the redundant first branching time, which equals the island
  # age, as it is already the header
  are_redundant <- t$colonists_branching_times$branching_times ==
    t$header$island_age
  t$colonists_branching_times <- t$colonists_branching_times[!are_redundant, ]

  #####################################################
  # Creata a table for drawing the horizontal branches
  #####################################################
  # Note we only have extant species
  n_branches_per_clade_index <- dplyr::summarise(
    dplyr::group_by(t$colonists_branching_times, clade_index),
    n = dplyr::n()
  )
  branches_horizontal <- t$colonists_branching_times
  # Determine the y coordinats per clade_index, space out the y's nicely
  branches_horizontal$y <- NA
  cur_clade_index <- 0
  delta_y <- 0
  y <- 0
  for (row_index in seq_along(branches_horizontal$branching_times)) {
    this_clade_index <- branches_horizontal$clade_index[row_index]
    if (this_clade_index != cur_clade_index) {
      # New clade_index
      cur_clade_index <- this_clade_index
      delta_y <- 1.0 / n_branches_per_clade_index[
        n_branches_per_clade_index$clade_index == cur_clade_index, ]$n
      y <- delta_y / 2.0
    } else {
      y <- y + delta_y
    }
    branches_horizontal$y[row_index] <- y
  }
  testthat::expect_true("branching_times" %in% names(branches_horizontal))
  #####################################################
  # Creata a table for drawing the vertical branches
  #####################################################
  # As the parent of a branch is unknown, use a comb graph
  branches_vertical <- branches_horizontal
  # Make vertical branches go to their parents
  # Parents have the y index above it
  # The parent branches will have nonsense values
  last_row_index <- length(branches_vertical$y)
  branches_vertical$yend <- c(0.0, branches_vertical$y[-last_row_index])
  # Use the branching times of the parents
  branches_vertical$branching_times <- c(
    0.0, branches_vertical$branching_times[-last_row_index]
  )

  # Get rid of the parents, i.e. those with the lowest y per clade_index
  branches_vertical <- dplyr::slice(
    dplyr::group_by(
      branches_vertical,
      clade_index
    ),
    -which.min(y)
  )




  # Only obtain the colonisations, i.e. the first branching time,
  # plot these as points with a shape depending on stac_str
  first_branching_times <- dplyr::slice(
    dplyr::group_by(
      branches_horizontal,
      clade_index
    ),
    which.min(branching_times)
  )
  colonisations <- merge(first_branching_times, t$colonists_general)

  p + ggplot2::geom_point(
    data = colonisations,
    ggplot2::aes(x = branching_times, y = y, shape = stac_str)
  ) + ggplot2::geom_segment(
    data = branches_horizontal,
    ggplot2::aes(
      x = branching_times,
      y = y,
      xend = t$header$island_age,
      yend = y
    )
  ) + ggplot2::geom_segment(
    data = branches_vertical,
    ggplot2::aes(
      x = branching_times,
      y = y,
      xend = branching_times,
      yend = yend
    )
  ) + ggplot2::facet_grid(clade_index ~ .)
}