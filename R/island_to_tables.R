#' Convert an `island` to a list of tables
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
island_to_tables <- function(island) {

  tables <- list()
  tables$empirical_island <- empirical_island_to_tables(island$empirical_island)
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
empirical_island_to_tables <- function(empirical_island) {
  # It is exactly the same
  DAISIEmainland::ideal_island_to_tables(empirical_island)
}

#' Convert an `ideal_island` to a list of tables
#' @inheritParams default_params_doc
#' @return a \link{list} with elements:
#'   * `speciations`: a table with the speciation events
#'   * `colonisations`: a table with colonisations
#' @export
ideal_island_to_tables <- function(ideal_island) {
  # Fix build warnings
  branching_times <- NULL; rm(branching_times) # nolint, fixes warning: no visible binding for global variable
  unique_species_id <- NULL; rm(unique_species_id) # nolint, fixes warning: no visible binding for global variable
  stac_str <- NULL; rm(stac_str) # nolint, fixes warning: no visible binding for global variable

  # Convert the list elements 'all_colonisations' to its own table
  all_colonisations <- DAISIEmainland::all_colonisations_to_table(
    ideal_or_empirical_island = ideal_island
  )
  # Remove the 'all_colonisations's, so that the remaining data can be
  # conerted to a table
  for (i in seq_along(ideal_island)) {
    if (is.null(ideal_island[[i]]$all_colonisations)) next
    ideal_island[[i]]$all_colonisations <- NULL
  }

  # Give each list element a clade id
  for (i in seq_along(ideal_island)) {
    ideal_island[[i]]$clade_id <- i
  }
  # Combine the list into one big tibble
  t <- dplyr::bind_rows(ideal_island)
  t$stac_str <- Vectorize(stac_to_str)(t$stac)
  t$stac_str <- as.factor(t$stac_str)

  # Number all species of all clades individually
  t$unique_species_id <- seq(1, nrow(t))
  t$unique_species_id <- as.factor(t$unique_species_id)

  list(
    speciations = t,
    colonisations = all_colonisations
  )
}
