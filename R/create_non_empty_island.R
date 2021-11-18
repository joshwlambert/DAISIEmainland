#' Creates an ideal and empirical data set for a single non empty island
#'
#' @inheritParams default_params_doc
#'
#' @return List with the island information, composed of
#' branching times of extant species, status of species on
#' the island and number of missing species.
#' @author Joshua W. Lambert
create_non_empty_island <- function(total_time,
                                    island_spec,
                                    mainland_clade,
                                    mainland_sample_prob,
                                    mainland_sample_type) {

  names(island_spec)[3] <- "col_t_bp"
  names(island_spec)[6] <- "branch_t_bp"
  # set ages as counting backwards from present
  island_spec[, "branch_t_bp"] <- total_time - island_spec[, "branch_t_bp"]
  island_spec[, "col_t_bp"] <- total_time - island_spec[, "col_t_bp"]

  # number of independent colonisations from different mainland species
  ideal_col_present <- sort(unique(island_spec[, "main_anc_id"]))
  num_ideal_col_present <- length(ideal_col_present)

  ideal_island <- list()
  for (i in seq_len(num_ideal_col_present)) {
    subset_island <- island_spec[which(island_spec[, "main_anc_id"] ==
                                         ideal_col_present[i]), ]

    ideal_island[[i]] <- create_ideal_island(
      total_time = total_time,
      island_spec = subset_island)
  }

  # adjust mainland object for sampling probability
  mainland_clade <- sample_mainland(
    total_time = total_time,
    mainland_clade = mainland_clade,
    mainland_sample_prob = mainland_sample_prob,
    mainland_sample_type = mainland_sample_type,
    island_spec = island_spec)

  island_spec <- update_island_endemics(
    timeval = total_time,
    total_time = total_time,
    island_spec = island_spec,
    mainland_clade = mainland_clade)

  # number of independent colonisations from different mainland species
  empirical_col_present <- sort(unique(island_spec[, "main_anc_id"]))

  # determine whether colonisations from different mainland species are more
  # closely related to each other than to the extant species on the mainland
  if (length(empirical_col_present) > 1) {
    empirical_col_present <- calc_empirical_col(
      empirical_col_present = empirical_col_present,
      island_spec = island_spec,
      mainland_clade = mainland_clade)
  }

  num_empirical_col_present <- length(empirical_col_present)

  empirical_island <- list()
  for (i in seq_len(num_empirical_col_present)) {
    subset_island <- island_spec[which(island_spec[, "main_anc_id"] ==
                                         empirical_col_present[[i]]), ]

    mainland_spec <-
      which(mainland_clade[, "spec_id"] %in% empirical_col_present[[i]])

    # is there any extant descendants of the immigrant on the mainland
    branching_code <-
      paste0("^", mainland_clade[mainland_spec, "branch_code"])

    descending_branches <- unique(unlist(lapply(
      branching_code,
      function(x) grep(x, mainland_clade[, "branch_code"])
    )))

    extant_mainland <-
      any(mainland_clade[descending_branches, "spec_type"] != "E" &
            mainland_clade[descending_branches, "spec_type"] != "US" &
            mainland_clade[descending_branches, "spec_type"] != "UD")

    # if there is an extant descendants of the immigrant on the mainland ideal
    # is the same as empirical, else empirical is different
    if (isTRUE(extant_mainland)) {
      empirical_island[[i]] <- create_ideal_island(
        total_time = total_time,
        island_spec = subset_island)
    } else if (isFALSE(extant_mainland)) {
      empirical_island[[i]] <- create_empirical_island(
        total_time = total_time,
        island_spec = subset_island,
        mainland_clade = mainland_clade,
        mainland_spec = mainland_spec)
    }
  }
  return(list(ideal_island = ideal_island,
              empirical_island = empirical_island))
}
