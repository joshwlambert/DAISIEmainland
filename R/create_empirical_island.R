create_empirical_island <- function(total_time,
                                    island_spec,
                                    mainland_clade,
                                    mainland_spec) {
  # number of independent colonisations from the same mainland species
  number_colonisations <-
    length(unique(island_spec[, "col_t_bp"]))
  # are there any branching events between the immig time and island
  # age with extant descendants
  other_extant_mainland <- any(mainland_clade[, "spec_type"] != "E" &
                                 mainland_clade[, "spec_type"] != "US" &
                                 mainland_clade[, "spec_type"] != "UD")

  if (number_colonisations == 1) {
    if (other_extant_mainland) {
      anc_branch_t_bp <- common_ancestor_time(
        total_time = total_time,
        mainland_spec = mainland_spec,
        mainland_clade = mainland_clade)
      brts <- unique(sort(island_spec[, "branch_t_bp"],
                          decreasing = TRUE))
      brts <- brts[-1]
      empirical_island <- list(
        branching_times = c(total_time,
                            anc_branch_t_bp,
                            brts),
        stac = 2,
        missing_species = 0)
    } else {
      if (nrow(island_spec) == 1) {
        if (mainland_clade[mainland_spec, "spec_type"] == "US") {
          empirical_island <- list(
            branching_times = c(total_time, total_time - 1e-5),
            stac = 1,
            missing_species = 0)
        } else {
          empirical_island <- list(
            branching_times = c(total_time, total_time - 1e-5),
            stac = 5,
            missing_species = 0)
        }
      } else {
        brts <- sort(island_spec[, "branch_t_bp"], decreasing = TRUE)
        brts <- brts[-1]
        empirical_island <- list(
          branching_times = c(total_time, total_time - 1e-5, brts),
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
        subset_island = island_spec)
      empirical_island <- list(
        branching_times = false_clade_brts,
        stac = 2,
        missing_species = 0)
    } else {
      false_clade_brts <- create_false_clade_brts(
        total_time = total_time,
        anc_branch_t_bp = total_time - 1e-5,
        subset_island = island_spec)
      empirical_island <- list(
        branching_times = false_clade_brts,
        stac = 6,
        missing_species = 0)
    }
  }
  return(empirical_island)
}
