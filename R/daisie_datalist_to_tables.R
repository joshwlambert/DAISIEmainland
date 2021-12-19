#' Convert an `daisie_datalist` to a list of tables
#'
#' @inheritParams default_params_doc
#'
#' @return a \link{list} with elements:
#'   * `island_age`: the island age
#'   * `not_present`:
#'   * `colonisations`: a table with all colonisations, with
#'     column names `colonist_index`, `branching_times`, `stac`
#'     and `missing_species`
#'
#'
#' @examples
#' island <- DAISIEmainland::sim_island_with_mainland(
#'   total_time = 1,
#'   m = 100,
#'   island_pars = c(1, 1, 50, 0.1, 1),
#'   mainland_ex = 0.5,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete",
#'   replicates = 1,
#'   verbose = FALSE
#' )
#' ideal_daisie_data <- island$ideal_islands
#' daisie_datalist <- ideal_daisie_data[[1]]
#' daisie_datalist_to_tables(daisie_datalist)
#' @author Richèl J.C. Bilderbeek
#'
#' @export
daisie_datalist_to_tables <- function(daisie_datalist) {
  DAISIEmainland::check_daisie_datalist(daisie_datalist)

  # The first element, the 'header' is special
  daisie_datalist_header <- daisie_datalist[[1]]
  testthat::expect_true("island_age" %in% names(daisie_datalist_header))
  # Could also be 'not_present_type1' and 'not_present_type1'
  # according to do. However, DAISIEmainland never uses this
  testthat::expect_true("not_present" %in% names(daisie_datalist_header))

  tables <- list()

  # '2' as the first element is special
  for (list_index in seq(2, length(daisie_datalist))) {
    # colonist_index '2' is the index of the first colonist
    colonist_lineage <- daisie_datalist[[list_index]]
    colonist_index <- list_index - 1
    table <- data.frame(
      colonist_index = colonist_index,
      branching_times = colonist_lineage$branching_times,
      stac = colonist_lineage$stac,
      missing_species = colonist_lineage$missing_species,
      stringsAsFactors = FALSE
    )
    tables[[colonist_index]] <- table
  }

  colonisations <- dplyr::bind_rows(tables)
  daisie_datalist_header$colonisations <- colonisations

  daisie_datalist_header
}
