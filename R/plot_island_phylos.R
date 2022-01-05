#' Plots island phylogenetic data
#'
#' @param island stub
#'
#' @return stub
#' @export
plot_island_phylos <- function(island) {

  # Fix build warnings
  x <- NULL; rm(x) # nolint, fixes warning: no visible binding for global variable
  y <- NULL; rm(y) # nolint, fixes warning: no visible binding for global variable
  xend <- NULL; rm(xend) # nolint, fixes warning: no visible binding for global variable
  yend <- NULL; rm(yend) # nolint, fixes warning: no visible binding for global variable
  root_length <- NULL; rm(root_length) # nolint, fixes warning: no visible binding for global variable

  island_phylos <- island_to_phylos(island = island)
  island_endemicity <- unlist(lapply(island[[1]], "[[", "stac"))
  island_age <- island[[1]][[1]]$island_age

  clades <- lapply(island_phylos, function(x) if (length(x$edge.length) > 1) x)
  drop_null <- which(sapply(clades, is.null))
  clades <- clades[-drop_null]

  singletons <- lapply(island_phylos, function(x) {
    if (length(x$edge.length) == 1) x
  })
  drop_null <- which(sapply(singletons, is.null))
  singletons <- singletons[-drop_null]

  singletons_tbl <- singleton_to_segment(singletons)

  clades_tbls <- lapply(clades, ggtree::fortify)
  # Give each list element a clade id
  for (i in seq_along(clades_tbls)) {
    clades_tbls[[i]]$clade_id <- i
    clades_tbls[[i]]$root_edge <- clades[[i]]$root.edge
    #island_phylos[[i]]$endemicity <- island_endemicity[i]
  }

  clades_bound <- dplyr::bind_rows(clades_tbls)

  clades_plot <- ggtree::ggtree(clades_bound) +
    ggplot2::facet_wrap(~ clade_id, ncol = 1) +
    ggtree::geom_rootedge() +
    ggtree::theme_tree2() +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank()) +
    ggplot2::geom_vline(xintercept = -island_age, lty = 2) +
    ggtree::geom_rootpoint(position = ggplot2::position_nudge(x = -root_length))

  singletons_plot <- ggplot2::ggplot(data = singletons_tbl) +
    ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    ggplot2::geom_point(ggplot2::aes(x = xend, y = yend)) +
    ggplot2::geom_vline(xintercept = -island_age, lty = 2) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.line = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank())

  cowplot::plot_grid(singletons_plot, clades_plot, ncol = 1)


}
