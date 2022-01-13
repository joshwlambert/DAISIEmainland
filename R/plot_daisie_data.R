#' Plot the `daisie_data`.
#'
#' @inheritParams default_params_doc
#'
#' @return a `ggplot2`
#'
#' @author Richèl J.C. Bilderbeek
#' @export
plot_daisie_data <- function(daisie_data) {
  t <- DAISIEmainland::daisie_data_to_tables(daisie_data)

  p <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(
      name = "Time",
      limits = c(0, t$header$island_age)
    )
  if (nrow(t$colonists_general) == 0) {
    return(p)
  }

   # Remove the redundant first branching time, which equals the island
   # age, as it is already the header
   are_redundant <- t$colonists_branching_times$branching_times == t$header$island_age
   t$colonists_branching_times <- t$colonists_branching_times[!are_redundant, ]


  # Only obtain the colonisations, i.e. the first branching time
  first_branching_times <- dplyr::slice(
    dplyr::group_by(
      t$colonists_branching_times,
      clade_index
    ),
    which.min(branching_times)
  )
  colonisations <- merge(first_branching_times, t$colonists_general)

  ggplot2::ggplot() +
    ggplot2::geom_point(
    data = colonisations,
    ggplot2::aes(x = branching_times, y = clade_index, shape = stac_str)
  ) + ggplot2::scale_x_continuous(
    name = "Time",
    limits = c(0, t$header$island_age)
  )
}
