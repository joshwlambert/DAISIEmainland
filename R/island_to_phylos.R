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
  testit::assert(is.list(island))
  if (length(island[[1]]) == 1) {
    stop("Empty island")
  }
  island_age <- island[[1]][[1]]$island_age
  island <- island[[1]][-1]
  branching_times <- lapply(island, "[[", "branching_times")
  stacs <- unlist(lapply(island, "[[", "stac"))
  branching_times_no_recol <- branching_times[which(stacs != 3)]
  phylos_no_recols <- lapply(branching_times_no_recol, function(x) {
    if (length(x) == 2) {
      create_singleton_phylo(x[2])
    } else if (length(x) > 2) {
      DDD::brts2phylo(x[-1], root = TRUE)
    } else {
      stop("no species in island clade")
    }
  })

  if (any(stacs == 3)) {
    recol <- island[which(stacs == 3)]
    all_cols <- lapply(recol, "[[", "all_colonisations")
    event_times <- lapply(all_cols,
                          function(x) {lapply(x, "[[", "event_times")})
    recol_phylos <- lapply(event_times, function(x) {
      lapply(x,  function(y) {
        if (length(y) == 2) {
          create_singleton_phylo(y[2])
        } else if (length(y) > 2) {
          DDD::brts2phylo(y[-1], root = TRUE)
        } else {
          stop("no species in island clade")
        }
      })
    })
    recol_phylos <- unlist(recol_phylos, recursive = FALSE)
    phylos <- c(phylos_no_recols, recol_phylos)
  } else {
    phylos <- phylos_no_recols
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
