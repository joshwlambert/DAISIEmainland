#' Plots island phylogenetic data
#'
#' @param island_phylos stub
#' @param island_age stub
#'
#' @return
#' @export
#'
#' @examples
plot_island_phylos <- function(island_phylos, island_endemicity, island_age) {


  root_length <- sapply(island_phylos, "[[", "root.edge")
  for (i in seq_along(island_phylos)) {
   island_phylos[[i]]$endemicity <- island_endemicity[i]
  }

  ggtree(island_phylos, aes(colour = "red")) +
    ggplot2::facet_wrap(~.id, ncol = 1) +
    ggtree::geom_rootedge(colour = "red") +
    theme_tree2() +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank()) +
    ggplot2::geom_vline(xintercept = -island_age, lty = 2) +
    ggtree::geom_rootpoint(position = ggplot2::position_nudge(x = -root_length))

  ### Add segments for island singletons
  # ggplot2::geom_segment(mapping = ggplot2::aes(x = , xend = , y = , yend = ))

}
