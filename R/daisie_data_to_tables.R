#' Convert an `daisie_data` to a list of tables
#'
#' @inheritParams default_params_doc
#'
#' @return a \link{list} with elements:
#'   * `ideal_island_table`: the `daisie_data$empirical_islands` as a table,
#'     as done by \link{empirical_daisie_data_to_tables}
#'   * `empirical_island_table`: the `daisie_data$ideal_island` as a table
#'     as done by \link{ideal_daisie_data_to_tables}
#'
#' @examples
#' set.seed(
#'   4,
#'   kind = "Mersenne-Twister",
#'   normal.kind = "Inversion",
#'   sample.kind = "Rejection"
#' )
#' daisie_data <- sim_island_with_mainland(
#'   total_time = 1.0,
#'   m = 10,
#'   island_pars = c(1, 1, 10, 0.1, 1),
#'   mainland_ex = 1,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete",
#'   replicates = 1)
#' daisie_data_to_tables(daisie_data)
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
daisie_data_to_tables <- function(daisie_data) {

  DAISIEmainland::check_daisie_data(daisie_data)
  tables <- list()
  tables$empirical_island <- DAISIEmainland::empirical_daisie_data_to_tables(
    empirical_daisie_data = daisie_data$empirical_islands)
  tables$empirical_island$data_type <- "empirical"
  tables$ideal_island <- DAISIEmainland::ideal_daisie_data_to_tables(
    daisie_data$ideal_island)
  tables$ideal_island$data_type <- "ideal"
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
  DAISIEmainland::check_ideal_daisie_data(ideal_daisie_data)

  tables <- list()
  for (i in seq_along(ideal_daisie_data)) {
    daisie_datalist <- ideal_daisie_data[[i]]
    table <- DAISIEmainland::daisie_datalist_to_tables(daisie_datalist)
    table$replicate <- i
    tables[[i]] <- table
  }
  dplyr::bind_rows(tables)
}
