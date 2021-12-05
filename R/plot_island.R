plot_island <- function(island) {
  t <- island_to_tables(island)

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
