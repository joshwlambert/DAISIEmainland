#' Extract all colonisations from an island history
#' and put these in a table
#' @inheritParams default_params_doc
#'
#' @return a table with column names:
#'  * `clade_id`: the clade ID
#'  * `colonist_id`: the colonists' ID
#'  * `event_times` ordered numeric vectors containing all
#'     events for each extant recolonising lineage. This includes all
#'     colonisation and branching times
#'  * `species_type` a string. Can be `"A"`, `"C"` or
#'     `"I"` depending on whether the extant clade is of anagenetic,
#'     cladogenetic or immigrant origin, respectively.
#'
#' @examples
#' mainland_clade <- DAISIEmainland:::create_test_mainland_clade(
#'   mainland_scenario = 20
#' )
#' island <- DAISIEmainland:::sim_island(
#'   total_time = 1,
#'   island_pars = c(1, 1, 10, 12, 1),
#'   mainland = mainland_clade,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete"
#' )
#' all_colonisations_to_table(ideal_or_empirical_island = island$ideal_island)
#' @author Richèl J.C. Bilderbeek
#'
#' @export
all_colonisations_to_table <- function(ideal_or_empirical_island) {             # nolint indeed a too complex function

  # Collect the 'ideal_or_empirical_island[[i]]$all_colonisations's
  # in one list without adding a dependency on purrr
  all_colonisations_list <- list()
  for (i in seq_along(ideal_or_empirical_island)) {
    if (is.null(ideal_or_empirical_island[[i]]$all_colonisations)) next
    all_colonisations_list[[i]] <-
      ideal_or_empirical_island[[i]]$all_colonisations
  }

  # Check doc:
  # For recolonising lineages, there is an extra element,
  # `all_colonisations` per list element.
  # It is comprised of `$event_times` and `$species_type`:
  # \describe{
  #   \item{`$event_times`}{ordered numeric vectors containing all
  #     events for each extant recolonising lineage. This includes all
  #     colonisation and branching times. Each vector pertains to one
  #     colonising lineage.}
  #   \item{`$species_type`}{a string. Can be `"A"`, `"C"` or
  #     `"I"` depending on whether the extant clade is of anagenetic,
  #     cladogenetic or immigrant origin, respectively.}
  # }
  for (clade_id in seq_along(all_colonisations_list)) {
    clade_all_colonisations_list <- all_colonisations_list[[clade_id]]
    # Unsure if it 'n_recolonising_lineages' is not a better name
    n_colonising_lineages <- length(clade_all_colonisations_list)
    for (lineage_id in seq_len(n_colonising_lineages)) {
      colonising_lineage <- clade_all_colonisations_list[[lineage_id]]
      testthat::expect_true("event_times" %in% names(colonising_lineage))
      testthat::expect_true("species_type" %in% names(colonising_lineage))
    }
  }
  all_colonisations_list

  # clade_id | event_times | species type
  tables <- list()
  for (clade_id in seq_along(all_colonisations_list)) {
    clade_colonisations_list <- all_colonisations_list[[clade_id]]
    if (is.null(clade_colonisations_list)) {
      next
    }
    clade_tables <- list()
    for (colonist_id in seq_along(clade_colonisations_list)) {
      colonist <- clade_colonisations_list[[colonist_id]]
      clade_table <- data.frame(
        colonist_id = colonist_id,
        event_times = colonist$event_times,
        species_type = colonist$species_type,
        stringsAsFactors = FALSE
      )
      clade_tables[[colonist_id]] <- clade_table
    }
    table <- dplyr::bind_rows(clade_tables)
    table$clade_id <- clade_id
    tables[[clade_id]] <- table
  }
  t <- dplyr::bind_rows(tables)
  if (nrow(t) == 0) {
    # Same column order and data type as a regular table
    t <- data.frame(
      colonist_id = integer(0),
      event_times = numeric(0),
      species_type = character(0),
      clade_id = integer(0),
      stringsAsFactors = FALSE
    )
  }
  testthat::expect_true("colonist_id" %in% names(t))
  testthat::expect_true("event_times" %in% names(t))
  testthat::expect_true("species_type" %in% names(t))
  testthat::expect_true("clade_id" %in% names(t))
  t
}
