#' Convert an `daisie_data` to a list of tables
#'
#' @inheritParams default_params_doc
#'
#' @return a \link{list} with elements:
#'   * `speciations`: a table with the speciation events
#'   * `colonisations`: a table with colonisations
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
daisie_data_to_tables <- function(daisie_data) {

  tables <- list()
  tables$empirical_island <- empirical_daisie_data_to_tables(
    empirical_daisie_data = daisie_data$empirical_islands
  )
  tables$empirical_island$speciations$data_type <- "empirical"
  if (nrow(tables$empirical_island$colonisations) != 0) {
    tables$empirical_island$colonisations$data_type <- "empirical"
  }
  tables$ideal_island <- ideal_island_to_tables(island$ideal_island)
  tables$ideal_island$speciations$data_type <- "ideal"
  if (nrow(tables$ideal_island$colonisations) != 0) {
    tables$ideal_island$colonisations$data_type <- "ideal"
  }
  tables$speciations <- dplyr::bind_rows(
    tables$empirical_island$speciations,
    tables$ideal_island$speciations
  )
  tables$colonisations <- dplyr::bind_rows(
    tables$empirical_island$colonisations,
    tables$ideal_island$colonisations
  )
  tables
}

#' Convert an `empirical_island` to a list of tables
#' @inheritParams default_params_doc
#' @return a \link{list} with elements:
#'   * `speciations`: a table with the speciation events
#'   * `colonisations`: a table with colonisations
#' @export
empirical_daisie_data_to_tables <- function(empirical_daisie_data) {
  # It is exactly the same
  DAISIEmainland::ideal_daisie_data_to_tables(
    ideal_daisie_data = empirical_daisie_data
  )
}

#' Convert an `ideal_island` to a list of tables
#' @inheritParams default_params_doc
#' @return a \link{list} with elements:
#'   * `speciations`: a table with the speciation events
#'   * `colonisations`: a table with colonisations
#' @export
ideal_daisie_data_to_tables <- function(ideal_daisie_data) {
  # It is always the same?
  testthat::expect_true(
    length(ideal_daisie_data) == 2 &&
      length(ideal_daisie_data[[1]]) == 1 &&
      length(ideal_daisie_data[[1]][[1]]) == 2 &&
      ideal_daisie_data[[1]][[1]]$island_age == total_time &&
      ideal_daisie_data[[1]][[1]]$not_present == 0 &&
      length(ideal_daisie_data[[2]]) == 1 &&
      length(ideal_daisie_data[[2]][[1]]) == 2 &&
      ideal_daisie_data[[2]][[1]]$island_age == total_time &&
      ideal_daisie_data[[2]][[1]]$not_present == 0
  )
  stop("No idea how to put this in a table yet")
}
