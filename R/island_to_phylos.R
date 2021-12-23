#' Converts a DAISIE datalist (either ideal or empirical) to a list of phylo
#' objects that can be used to plot the island.
#'
#' @inheritParams default_params_doc
#'
#' @return List of phylo objects. See ape R package for details on phylo class
#' @export
#'
#' @examples
#' island <- sim_island_with_mainland(
#'   total_time = 1,
#'   m = 100,
#'   island_pars = c(1,1,10,0.1,1),
#'   mainland_ex = 1,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete",
#'   replicates = 1)
#' island_phylos <- island_to_phylos(island = island$ideal_islands)
island_to_phylos <- function(island) {
  browser()
  testit::assert(is.list(island))
  if (length(island[[1]]) == 1) {
    stop("Empty island")
  }
  island_age <- island[[1]][[1]]$island_age
  island <- island[[1]][-1]
  all_times <- lapply(island, "[[", "branching_times")
  phylos <- lapply(all_times, function(x) {
    if (length(x) == 2) {
      create_singleton_phylo(x[2])
    } else if (length(x) > 2) {
      branching_times <- x[-1]
      DDD::brts2phylo(branching_times, root = TRUE)
    } else {
      stop("no species in island clade")
    }
  })

  stacs <- unlist(lapply(island, "[[", "stac"))
  if (any(stacs == 3)) {
    recol <- island[which(stacs == 3)]
    all_cols <- lapply(recol, "[[", "all_colonisations")
    event_times <- lapply(all_cols,
                          function(x) {lapply(x, "[[", "event_times")})
    col_times <- lapply(event_times, "[[", 2)
    branching_times <- lapply(event_times, function(x) {
      lapply(x, function(y)  {if (length(y) > 2) return(y[-c(1)])})
    })
    stac_3_phylos <- lapply(branching_times, function(x) {
      lapply(x,  function(y) {if (!is.null(x)) DDD::brts2phylo(x, root = TRUE)})
    })
    null_phylos <- which(sapply(stac_3_phylos, is.null))
    stac_3_phylos <- stac_3_phylos[-null_phylos]
    phylos <- c(phylos, stac_3_phylos)
  }
  if (length(phylos) == 1) {
    class(phylos) <- "phylo"
  } else if (length(phylos) > 1) {
    class(phylos) <- "multiPhylo"
  } else {
    stop("No phylogenies on the island")
  }
  return(phylos)
}
