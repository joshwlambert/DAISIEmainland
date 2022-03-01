#' Plot the `daisie_data`.
#'
#' Plot the `daisie_data`.
#'
#' Each colonisation and re-colonisation event has its own branching
#' dynamics. For clades that start from anagenesis, these are marked as such,
#' until their first cladogenesis event. Due to this, it may seem that
#' anagenetic clades do not speciate, but this is false: they are plotted
#' as such until a (cladegenetic) speciation event.
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
  colonisation_time <- NULL; rm(colonisation_time) # nolint, fixes warning: no visible binding for global variable

  t <- DAISIEmainland::daisie_data_to_tables(daisie_data)

  p <- ggplot2::ggplot(t$colonists_general) +
    ggplot2::scale_x_reverse(
      name = "Time before present",
      limits = c(t$header$island_age, 0)
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  # Sometimes, no colonisations take place, making an island history dull ...
  if (nrow(t$colonists_general) == 0) {
    return(p)
  }
  #####################################################
  # Create a table for drawing the horizontal branches
  #####################################################
  n_colonists_per_clade_index <- dplyr::summarise(
    dplyr::group_by(t$colonisation_times, clade_index),
    n_colonists = dplyr::n()
  )
  n_branches_per_clade_index <- dplyr::summarise(
    dplyr::group_by(t$colonists_branching_times, clade_index, colonist_index),
    n = dplyr::n(),
    .groups = "drop"
  )
  t$colonisation_times$y <- t$colonisation_times$colonist_index - 0.5

  branches_horizontal_from_colonists <- tibble::tibble(
    clade_index = t$colonisation_times$clade_index,
    colonist_index = t$colonisation_times$colonist_index,
    x = t$colonisation_times$colonisation_time,
    xend = 0, # the present
    y = NA, # unknown now
    colonist_species_type = t$colonisation_times$colonist_species_type
  )
  branches_horizontal_from_branches <- tibble::tibble(
    clade_index = t$colonists_branching_times$clade_index,
    colonist_index = t$colonists_branching_times$colonist_index,
    x = t$colonists_branching_times$branching_times,
    xend = 0, # the present
    y = NA, # unknown now
    colonist_species_type = "Irrelevant"
  )
  # Add the colonist_species_type to branches_horizontal_from_branches
  # branches_horizontal_from_branches$colonist_species_type <- dplyr::inner_join(
  #   dplyr::select(
  #     branches_horizontal_from_colonists,
  #     clade_index,
  #     colonist_index,
  #     colonist_species_type
  #   ),
  #   dplyr::select(
  #     branches_horizontal_from_branches,
  #     clade_index,
  #     colonist_index
  #   ),
  #   by = c("clade_index", "colonist_index")
  # )$colonist_species_type
  unsorted_branches_horizontal <- dplyr::bind_rows(
    branches_horizontal_from_colonists,
    branches_horizontal_from_branches
  )
  branches_horizontal <- dplyr::arrange(
    unsorted_branches_horizontal,
    clade_index,
    colonist_index
  )

  # Determine the y coordinats per clade_index, space out the y's nicely
  branches_horizontal$y <- NA
  cur_clade_index <- 0
  cur_colonist_index <- 0
  delta_y <- 0
  y <- 0
  for (row_index in seq_len(nrow(branches_horizontal))) {
    this_clade_index <- branches_horizontal$clade_index[row_index]
    this_colonist_index <- branches_horizontal$colonist_index[row_index]
    if (this_clade_index != cur_clade_index || this_colonist_index != cur_colonist_index) {
      # New clade_index
      cur_clade_index <- this_clade_index
      cur_colonist_index <- this_colonist_index
      n_branches <- n_branches_per_clade_index[
        n_branches_per_clade_index$clade_index == cur_clade_index &
          n_branches_per_clade_index$colonist_index == cur_colonist_index
        , ]$n
      if (length(n_branches) == 0) n_branches <- 0
      delta_y <- 1.0 / (1.0 + n_branches)
      y <- delta_y / 2.0
    } else {
      y <- y + delta_y
    }
    branches_horizontal$y[row_index] <- y
  }
  branches_horizontal
  if (1 == 2) {
    # Note we only have extant species
    branches_horizontal <- tibble::tibble(
      clade_index = t$colonists_branching_times$CLADE_INDEX,
      colonist_index = t$colonisation_times$colonist_index,
      x = t$colonisation_times$colonisation_time,
      xmax = t$header$island_age
    )

    # Add the stac_str
    branches_horizontal <- merge(branches_horizontal, t$colonists_general)
    testthat::expect_true("branching_times" %in% names(branches_horizontal))
    testthat::expect_true("clade_index" %in% names(branches_horizontal))
    testthat::expect_true("y" %in% names(branches_horizontal))
    testthat::expect_true("stac_str" %in% names(branches_horizontal))
  }
  #####################################################
  # Creata a table for drawing the vertical branches
  #####################################################
  if (1 == 2) {
    # As the parent of a branch is unknown, use a comb graph
    branches_vertical <- branches_horizontal
    # Make vertical branches go to their parents
    # Parents have the y index above it
    # The parent branches will have nonsense values
    last_row_index <- length(branches_vertical$y)
    branches_vertical$yend <- c(0.0, branches_vertical$y[-last_row_index])
    # Use the branching times of the parents
    branches_vertical$branching_times <- c(
      0.0, branches_vertical$branching_times[-1]
    )

    # Get rid of the parents, i.e. those with the lowest y per clade_index
    branches_vertical <- dplyr::slice(
      dplyr::group_by(
        branches_vertical,
        clade_index
      ),
      -which.min(y)
    )
    # Add the stac_str
    branches_vertical <- merge(branches_vertical, t$colonists_general)
    testthat::expect_true("branching_times" %in% names(branches_vertical))
    testthat::expect_true("clade_index" %in% names(branches_vertical))
    testthat::expect_true("y" %in% names(branches_vertical))
    testthat::expect_true("yend" %in% names(branches_vertical))
    testthat::expect_true("stac_str" %in% names(branches_vertical))
  }


  colonisations <- dplyr::slice(
    dplyr::group_by(
      dplyr::select(branches_horizontal, clade_index, colonist_index, x, y, colonist_species_type),
      clade_index,
      colonist_index
    ),
    which.min(y)
  )

  p <- p + ggplot2::geom_point(
    data = colonisations,
    ggplot2::aes(
      x = x,
      y = y,
      shape = colonist_species_type,
      color = colonist_species_type
    )
  ) + ggplot2::geom_segment(
    data = branches_horizontal,
    ggplot2::aes(
      x = x,
      y = y,
      xend = xend,
      yend = y
    )
  )
  # + ggplot2::geom_segment(
  #   data = branches_vertical,
  #   ggplot2::aes(
  #     x = branching_times,
  #     y = y,
  #     xend = branching_times,
  #     yend = yend,
  #     color = stac_str
  #   )
  # )
  p + ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank()
    ) +
    ggplot2::facet_grid(clade_index ~ .)

  p + ggplot2::facet_grid(clade_index ~ .)
}
