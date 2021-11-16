#' Formats simulation output into standard DAISIE list output.
#'
#' @inheritParams default_params_doc
#'
#' @return List with DAISIE simulation output
#' @keywords internal
#' @author Joshua W. Lambert
format_to_daisie_data_core <- function(island_replicates,
                                       total_time,
                                       m) {

  several_islands <- list()
  for (rep in seq_along(island_replicates)) {
    full_list <- island_replicates[[rep]]
    ### separate taxon_list lists from empty island lists
    new_full_list <- list()
    for (i in seq_along(full_list)) {
      new_full_list <- append(new_full_list, full_list[[i]])
    }
    stac_vec <-
      unlist(new_full_list)[which(names(unlist(new_full_list)) == "stac")]
    number_not_present <- length(which(stac_vec == 0))
    present <- which(stac_vec != 0)
    number_present <- length(present)

    island_list <- list()
    island_list[[1]] <- list(island_age = total_time,
                             not_present = number_not_present)
    if (number_present > 0) {
      for (i in seq_len(number_present)) {
        island_list[[1 + i]] <- new_full_list[[present[i]]]
      }
    }
    several_islands[[rep]] <- island_list
  }
  return(several_islands)
}
