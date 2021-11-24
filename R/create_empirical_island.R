#' Convert intermediate output to final simulation output for empirical data
#'
#' @inheritParams default_params_doc
#'
#' @return a list with these elements:
#' \enumerate{
#'   \item `branching_times`, a sorted numeric vector, as required
#'     by the ML estimation functions. The first element always refers to
#'     the island age. Subsequent elements refer to colonisation, speciation and
#'     recolonisation times. The most recent recolonisation time, if any is
#'     always omitted to approximate simulation results to the mathematical
#'     formulation of the likelihood functions used for MLE.
#'   \item `stac`, status of colonist. In this function it can be
#'     returned as either 2, 4 or 3. If `stac` is 2, then there is only one
#'     independent colonisation present on the island and the extant species are
#'     endemic. If stac is 4, then only a singleton endemic is present at the
#'     present. If stac is 3, then recolonisation occurred, and more than one
#'     colonising lineage.
#'   \item `missing_species`, a numeric value with the number of
#'     missing species, that is, species not sampled in the phylogeny but
#'     present on the island. As this code only runs for simulation models,
#'     here `missing_species` is always set to 0.
#'   \item `all_colonisations`, on recolonising lineages only. It is
#'     comprised of `$event_times` and `$species_type`:
#'     \describe{
#'       \item{`$event_times`}{ordered numeric vectors containing all
#'       events for each extant recolonising lineage. This includes all
#'       colonisation and branching times. Each vector pertains to one
#'       colonising lineage.}
#'       \item{`$species_type`}{a string. Can be `"A"`, `"C"` or
#'       `"I"` depending on whether the extant clade is of anagenetic,
#'       cladogenetic or immigrant origin, respectively.}
#'   }
#' }
#' @keywords internal
#' @author Joshua W. Lambert
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
