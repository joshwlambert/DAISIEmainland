new_plot_island <- function(total_time,
                            island_tbl) {

  # give y position to each species
  for (y in seq_len(nrow(island_tbl))) {
    if (island_tbl$spec_type[y] %in% c("I", "A")) {
      island_tbl$y[y] <- y
    } else {
      island_tbl$branch_code[y] <- paste0("A", island_tbl$branch_code[y])
      island_tbl$y[y] <- branch_code_to_y(island_tbl$branch_code[y])
    }
  }


  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = island_tbl,
      ggplot2::aes(
        x = col_t,
        xend = total_time,
        y = y,
        yend = y
      )
    ) +
    ggplot2::xlim(c(0, total_time))

  ggplot2::ggplot(island_tbl) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = col_t,
        xend = total_time,
        y = y,
        yend = y)) +
  ggplot2::theme_classic() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.line.y = ggplot2::element_blank()) +
    ggplot2::xlab("Mya")



  # give y position to each species
  island_tbl$y <- Vectorize(DAISIEmainland::branch_code_to_y)(
    island_tbl$branch_code
  )

  # Create a table for the vertical lines
  # x1 = spec_ex_t (of ancestor)
  # x2 = spec_origin_t (of derived)
  # y1 = y of branch_code ancestor
  # y2 = y of branch_code of derived species
  #
  # Work backwards
  t_ancestors <- data.frame(
    ancestor_branch_code = island_tbl$branch_code,
    ancestor_unique_species_id = island_tbl$spec_id,
    ancestor_spec_ex_t = total_time
  )
  # Make branching codes codes consistent with mainland branching codes
  for (i in seq_along(island_tbl$branch_code)) {
    if (is.na(island_tbl$branch_code[i])) {
      island_tbl$branch_code[i] <- "A"
    } else {
      island_tbl$branch_code[i] <- paste0("A", island_tbl$branch_code[i])
    }
  }

  for (unique_cols in unique(island_tbl$spec_id)) {
    print(island_tbl[which(island_tbl$spec_id == unique_cols), ])
  }

}
