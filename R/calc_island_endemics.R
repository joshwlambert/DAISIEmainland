#' Calculates the number of endemic and non-endemics species on a single island
#' replicate
#'
#' @inheritParams default_params_doc
#' @return List of two numerics
#' @author Joshua W. Lambert
calc_island_endemics <- function(daisie_data) {
  # remove the meta data from the daisie data
  daisie_data <- daisie_data[-1]
  branching_times <- lapply(daisie_data, "[[", "branching_times")
  num_spec <- lapply(branching_times, function(x) {length(x) - 1})
  stacs <- unlist(lapply(daisie_data, "[[", "stac"))
  num_endemics <- sum(unlist(num_spec[which(stacs %in% c(2, 5, 6))]))
  num_non_endemics <- sum(unlist(num_spec[which(stacs %in% c(1, 4))]))
  if (any(stacs == 3)) {
    recol <- daisie_data[which(stacs == 3)]
    all_cols <- lapply(recol, "[[", "all_colonisations")
    event_times <- lapply(all_cols,
                          function(x) {lapply(x, "[[", "event_times")})
    num_recol_spec <- unlist(
      lapply(event_times, function(x) {lapply(x, function(y) length(y) - 1)})
    )
    species_type <- unlist(
      lapply(all_cols, function(x) {lapply(x, "[[", "species_type")})
    )
    endemics <- which(species_type %in% c("C", "A"))
    non_endemics <- which(species_type %in% c("I"))
    num_recol_endemics <- sum(num_recol_spec[endemics])
    num_recol_non_endemics <- sum(num_recol_spec[non_endemics])
    num_endemics <- num_endemics + num_recol_endemics
    num_non_endemics <- num_non_endemics + num_recol_non_endemics
  }
  testit::assert(num_endemics >= 0)
  testit::assert(num_non_endemics >= 0)
  return(list(endemics = num_endemics,
              non_endemics = num_non_endemics))
}
