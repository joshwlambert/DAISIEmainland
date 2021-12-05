#' Convert an `island` to a list of tables
#' @inheritParams default_params_doc
#' @return a \link{list} with elements:
#'   * `t`: a table
#'   * `empirical_island$all_colonisations`: a list with all colonizations
#'     in the empirical data
#'   * `ideal_island$all_colonisations`: a list with all colonizations
#'     in the ideal data
#' @export
island_to_tables <- function(island) {

  tables <- list()
  tables$empirical_island <- empirical_island_to_tables(island$empirical_island)
  tables$empirical_island$t$data_type <- "empirical"
  tables$ideal_island <- ideal_island_to_tables(island$ideal_island)
  tables$ideal_island$t$data_type <- "ideal"
  tables$t <- dplyr::bind_rows(tables$empirical_island$t, tables$ideal_island$t)
  tables$empirical_island$t <- NULL
  tables$ideal_island$t <- NULL
  tables$t$data_type <- as.factor(tables$t$data_type)
  tables
}

#' Convert an `empirical_island` to a list of tables
#' @inheritParams default_params_doc
#' @return a \link{list} with elements:
#'   * `t`: a table
#'   * `all_colonisations`: a list with all colonizations
#' @export
empirical_island_to_tables <- function(empirical_island) {
  # It is exactly the same
  DAISIEmainland::ideal_island_to_tables(empirical_island)
}

#' Convert an `ideal_island` to a list of tables
#' @inheritParams default_params_doc
#' @return a \link{list} with elements:
#'   * `t`: a table
#'   * `all_colonisations`: a list with all colonizations
#' @export
ideal_island_to_tables <- function(ideal_island) {
  # Fix build warnings
  branching_times <- NULL; rm(branching_times) # nolint, fixes warning: no visible binding for global variable
  unique_species_id <- NULL; rm(unique_species_id) # nolint, fixes warning: no visible binding for global variable
  stac_str <- NULL; rm(stac_str) # nolint, fixes warning: no visible binding for global variable

  # Move 'ideal_island$all_colonisations' to a seperate list
  all_colonisations <- list()
  for (i in seq_along(ideal_island)) {
    if (is.null(ideal_island[[i]]$all_colonisations)) next
    all_colonisations[[i]] <- ideal_island[[i]]$all_colonisations
    all_colonisations[[i]]$clade_id <- i
    ideal_island[[i]]$all_colonisations <- NULL
  }
  if (1 == 2) {
    # This is too complex for now
    dplyr::bind_rows(all_colonisations)
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
    t = t,
    all_colonisations = all_colonisations
  )
}
