#' Creates an ideal and empirical data set for a single non empty island
#'
#' @inheritParams default_params_doc
#'
#' @return List with the island information, composed of
#' branching times of extant species, status of species on
#' the island and number of missing species.
create_non_empty_island <- function(
  total_time,
  island_spec,
  mainland_clade,
  mainland_sample_prob) {

  names(island_spec)[3] <- "col_t_bp"
  names(island_spec)[6] <- "branch_t_bp"
  ### set ages as counting backwards from present
  island_spec[, "branch_t_bp"] <- total_time - island_spec[, "branch_t_bp"]
  island_spec[, "col_t_bp"] <- total_time - island_spec[, "col_t_bp"]

  ### number of independent colonisations from different mainland species
  colonists_present <- sort(unique(island_spec[, "main_anc_id"]))
  number_colonists_present <- length(colonists_present)

  ### adjust mainland object for sampling probability
  mainland_clade <- sample_mainland(
    total_time = total_time,
    mainland_clade = mainland_clade,
    mainland_sample_prob = mainland_sample_prob,
    island_spec = island_spec)

  island_spec <- update_island_endemics(
    timeval = total_time,
    total_time = total_time,
    island_spec = island_spec,
    mainland_clade = mainland_clade)

  ideal_island <- list()
  empirical_island <- list()

  for (i in 1:number_colonists_present) {
    subset_island <- island_spec[which(island_spec[, "main_anc_id"] ==
                                         colonists_present[i]), ]

    ideal_island[[i]] <- empirical_island[[i]] <-
      create_ideal_island(
        total_time = total_time,
        island_spec = subset_island)

    mainland_spec <-
      which(mainland_clade[, "spec_id"] == colonists_present[i])
    ### is there any extant descendants of the immigrant on the mainland
    branching_code <-
      paste0("^", mainland_clade[mainland_spec, "branch_code"])
    descending_branches <-
      grep(branching_code, mainland_clade[, "branch_code"])
    extant_mainland <-
      any(mainland_clade[descending_branches, "spec_type"] != "E" &
            mainland_clade[descending_branches, "spec_type"] != "NS")

    if (extant_mainland == FALSE) {
      ### number of independent colonisations from the same mainland species
      number_colonisations <-
        length(unique(subset_island[, "col_t_bp"]))
      ### are there any branching events between the immig time and island
      ### age with extant descendants
      other_extant_mainland <- any(mainland_clade[, "spec_type"] != "E" &
                                     mainland_clade[, "spec_type"] != "NS")
      if (number_colonisations == 1) {
        if (other_extant_mainland) {
          anc_branch_t_bp <- common_ancestor_time(
            total_time = total_time,
            mainland_spec = mainland_spec,
            mainland_clade = mainland_clade)
          empirical_island[[i]] <- list(
            branching_times = c(
              total_time,
              anc_branch_t_bp,
              sort(
                subset_island[, "branch_t_bp"],
                decreasing = TRUE)
            ),
            stac = 2,
            missing_species = 0)
        } else {
          if (nrow(subset_island) == 1) {
            empirical_island[[i]] <- list(
              branching_times = c(
                total_time,
                total_time - 1e-5),
              stac = 5,
              missing_species = 0)
          } else {
            empirical_island[[i]] <- list(
              branching_times = c(
                total_time,
                total_time - 1e-5,
                sort(
                  subset_island[, "branch_t_bp"],
                  decreasing = TRUE)
              ),
              stac = 6,
              missing_species = 0)
          }
        }
      } else if (number_colonisations > 1) {
        if (other_extant_mainland) {
          anc_branch_t_bp <- common_ancestor_time(
            total_time = total_time,
            mainland_spec = mainland_spec,
            mainland_clade = mainland_clade)
          false_clade_brts <- create_false_clade_brts(
            total_time = total_time,
            anc_branch_t_bp = anc_branch_t_bp,
            subset_island = subset_island)
          empirical_island[[i]] <- list(
            branching_times = false_clade_brts,
            stac = 2,
            missing_species = 0)
        } else {
          empirical_island[[i]] <- list(
            branching_times = c(
              total_time,
              total_time - 1e-5,
              sort(
                as.numeric(subset_island[, "branch_t_bp"]),
                decreasing = TRUE)
            ),
            stac = 6,
            missing_species = 0)
        }
      }
    }
  }
  return(list(ideal_island = ideal_island,
              empirical_island = empirical_island))
}
