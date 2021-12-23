#' Plots island phylogenetic data
#'
#' @param island_phylos stub
#' @param island_age stub
#'
#' @return
#' @export
#'
#' @examples
plot_island_phylos <- function(island) {

  island_phylos <- island_to_phylos(island = island)
  island_endemicity <- unlist(lapply(island[[1]], "[[", "stac"))
  island_age <- island[[1]][[1]]$island_age

  root_length <- sapply(island_phylos, "[[", "root.edge")
  #for (i in seq_along(island_phylos)) {
   #island_phylos[[i]]$endemicity <- island_endemicity[i]
  #}

  clades <- lapply(island_phylos, function(x) if (length(x$edge.length) > 1) x)
  drop_null <- which(sapply(clades, is.null))
  clades <- clades[-drop_null]

  singletons <- lapply(island_phylos, function(x) {
    if (length(x$edge.length) == 1) x
  })
  drop_null <- which(sapply(singletons, is.null))
  singletons <- singletons[-drop_null]


  clades_tbls <- lapply(clades, ggtree::fortify)
  # Give each list element a clade id
  for (i in seq_along(clades_tbls)) {
    clades_tbls[[i]]$clade_id <- i
  }

  clades_bound <- dplyr::bind_rows(clades_tbl)

  ggtree::ggtree(clades_bound, ggplot2::aes(colour = "red")) +
    ggplot2::facet_wrap(~.clade_id, ncol = 1) +
    ggtree::geom_rootedge(colour = "red") +
    ggtree::theme_tree2() +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank()) +
    ggplot2::geom_vline(xintercept = -island_age, lty = 2) +
    ggtree::geom_rootpoint(position = ggplot2::position_nudge(x = -root_length))


  segments <- singletons_to_segment()



  ### Add segments for island singletons
  # ggplot2::geom_segment(mapping = ggplot2::aes(x = , xend = , y = , yend = ))

}
