#' Converts simulation output into island output
#'
#' @inheritParams default_params_doc
#'
#' @return List with the island information, composed of
#' branching times of extant species, status of species on
#' the island and number of missing species.
#' @keywords internal
create_island <- function(total_time,
                          island_spec,
                          mainland,
                          mainland_sample_prob) {
  ### if there are no species on the island branching_times = island_age,
  ### stac = 0, missing_species = 0
  if (length(island_spec[, 1]) == 0) {
    ideal_island <- empirical_island <-
      list(branching_times = total_time,
           stac = 0,
           missing_species = 0)
  } else {
    cnames <- c("Species",
                "Mainland Ancestor",
                "Colonisation time (BP)",
                "Species type",
                "branch_code",
                "branching time (BP)",
                "Anagenetic_origin")
    colnames(island_spec) <- cnames
    ### set ages as counting backwards from present
    island_spec[, "branching time (BP)"] <- total_time -
      as.numeric(island_spec[, "branching time (BP)"])
    island_spec[, "Colonisation time (BP)"] <- total_time -
      as.numeric(island_spec[, "Colonisation time (BP)"])

    ### number of independent colonisations from different mainland species
    colonists_present <- sort(as.numeric(unique(
      island_spec[, "Mainland Ancestor"])))
    number_colonists_present <- length(colonists_present)

    ### adjust mainland object for sampling probability
    mainland <- sample_mainland(
      total_time = total_time,
      mainland = mainland,
      mainland_sample_prob = mainland_sample_prob,
      island_spec = island_spec)

    island_spec <- update_island_endemics(
      timeval = total_time,
      total_time = total_time,
      island_spec = island_spec,
      mainland = mainland)

    ideal_island_clades_info <- list()
    empirical_island_clades_info <- list()

    for (i in 1:number_colonists_present) {
      subset_island <- island_spec[which(island_spec[, "Mainland Ancestor"] ==
                                           colonists_present[i]), ]
      if (!is.matrix(subset_island)) {
        subset_island <- rbind(subset_island[1:7])
        colnames(subset_island) <- cnames
      }

      ideal_island_clades_info[[i]] <- create_island_core(
        time = total_time,
        island_spec = subset_island)

      mainland_spec <- which(mainland[, 1] == colonists_present[i])
      ### is there any extant descendants of the immigrant on the mainland
      branching_code <- paste("^", mainland[mainland_spec, 5], sep = "")
      descending_branches <- grep(branching_code, mainland[, 5])
      extant_mainland <- any(mainland[descending_branches, 4] != "E")

      if (extant_mainland) {
        empirical_island_clades_info[[i]] <- ideal_island_clades_info[[i]]
      } else {
        ### number of independent colonisations from the same mainland species
        number_colonisations <-
          length(unique(subset_island[, "Colonisation time (BP)"]))
        ### are there any branching events between the immig time and island
        ### age with extant descendants
        other_spec <- seq(from = 1, to = nrow(mainland), by = 1)[-mainland_spec]
        other_extant_mainland <- any(mainland[other_spec, 4] != "E")
        if (number_colonisations == 1) {
          if (other_extant_mainland) {
            branching_time <- common_ancestor_time(
              total_time = total_time,
              mainland_spec = mainland_spec,
              mainland = mainland)
            empirical_island_clades_info[[i]] <- list(
              branching_times = c(
                total_time,
                branching_time,
                sort(
                  as.numeric(subset_island[, "branching time (BP)"]),
                  decreasing = TRUE)
              ),
              stac = 2,
              missing_species = 0)
          } else {
            if (nrow(subset_island) == 1) {
              empirical_island_clades_info[[i]] <- list(
                branching_times = c(
                  total_time,
                  total_time - 1e-5),
                stac = 5,
                missing_species = 0)
            } else {
              empirical_island_clades_info[[i]] <- list(
                branching_times = c(
                  total_time,
                  total_time - 1e-5,
                  sort(
                    as.numeric(subset_island[, "branching time (BP)"]),
                    decreasing = TRUE)
                ),
                stac = 6,
                missing_species = 0)
            }
          }
        } else {
          if (other_extant_mainland) {
            branching_time <- common_ancestor_time(
              total_time = total_time,
              mainland_spec = mainland_spec,
              mainland = mainland)
            empirical_island_clades_info[[i]] <- list(
              branching_times = c(
                total_time,
                branching_time),
              stac = 3,
              missing_species = 0)
          } else {
            empirical_island_clades_info[[i]] <- list(
              branching_times = c(
                total_time,
                total_time - 1e-5,
                sort(
                  as.numeric(subset_island[, "branching time (BP)"]),
                  decreasing = TRUE)
              ),
              stac = 6,
              missing_species = 0)
          }
        }
      }
    }
    ideal_island <- ideal_island_clades_info
    empirical_island <- empirical_island_clades_info
  }
  return(list(ideal_island = ideal_island,
              empirical_island = empirical_island))
}
