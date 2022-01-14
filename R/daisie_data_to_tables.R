#' Convert `daisie_data` into a tabular format
#' @inheritParams default_params_doc
#'
#' @return a list with two elements:
#'
#'  * `header`: general parameters of the results
#'     see \link{daisie_header_to_table}
#'  * `colonists_general`: general info of each clade
#'     see \link{daisie_data_colonist_info_to_general_table}
#'  * `colonists_branching_times`: branching times per clade
#'     see \link{daisie_data_colonist_info_to_braching_times_table}
#' @author Richèl J.C. Bilderbeek
#'
#' @export
daisie_data_to_tables <- function(daisie_data) {
  DAISIEmainland::check_daisie_data(daisie_data)

  header <- DAISIEmainland::daisie_header_to_table(
    daisie_data_header = daisie_data[[1]])

  if (length(daisie_data) == 1) {
    return(
      list(
        header = header,
        colonists_general = data.frame(),
        colonists_branching_times = data.frame()
      )
    )
  }

  colonists_general_list <- list()
  colonists_branching_times_list <- list()

  # Due to the header, the first useful index is 2
  for (index in seq(2, length(daisie_data))) {
    t_g <- daisie_data_colonist_info_to_general_table(
      daisie_data_colonist_info = daisie_data[[index]]
    )
    t_g$clade_index <- index - 1 # Make indices start at 1, as users expect
    colonists_general_list[[index]] <- t_g

    t_bt <- daisie_data_colonist_info_to_braching_times_table(
      daisie_data_colonist_info = daisie_data[[index]]
    )
    t_bt$clade_index <- index - 1 # Make indices start at 1, as users expect
    colonists_branching_times_list[[index]] <- t_bt
  }

  colonists_general <- dplyr::bind_rows(colonists_general_list)
  colonists_branching_times <- dplyr::bind_rows(colonists_branching_times_list)

  list(
    header = DAISIEmainland::daisie_header_to_table(
      daisie_data_header = daisie_data[[1]]),
    colonists_general = colonists_general,
    colonists_branching_times = colonists_branching_times
  )
}

#' Convert a `daisie_data_header` into a tabular format
#'
#' @param daisie_data_header the first element of a `daisie_data`
#'
#' @return a table with one row,
#' containing all elements in a `daisie_data_header`
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
daisie_header_to_table <- function(daisie_data_header) {
  testthat::expect_equal(
    c("island_age", "not_present"),
    names(daisie_data_header)
  )
  data.frame(
    island_age = daisie_data_header$island_age,
    not_present = daisie_data_header$not_present,
    stringsAsFactors = FALSE
  )
}

#' Internal function
#'
#' Convert the header of a `daisie_data` into a tabular format
#' @param daisie_data_colonist_info an element of a `daisie_data`,
#' that is not the first element (the first element is of type
#' `daisie_data_header`).
#'
#' @return a table with as much rows as branching times
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
daisie_data_colonist_info_to_braching_times_table <- function( # nolint indeed a long function name
  daisie_data_colonist_info
) {
  data.frame(
    branching_times = daisie_data_colonist_info$branching_times,
    stringsAsFactors = FALSE
  )
}

#' Internal function
#'
#' Convert the header of a `daisie_data` into a tabular format
#' @param daisie_data_colonist_info an element of a `daisie_data`,
#' that is not the first element (the first element is of type
#' `daisie_data_header`).
#'
#' @return a table with one row,
#' containing all elements in a `daisie_data_colonist_info` header
#' that are not branching times.
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
daisie_data_colonist_info_to_general_table <- function( # nolint indeed a long internal function name
  daisie_data_colonist_info
) {
  data.frame(
    stac_str = stac_to_str(stac = daisie_data_colonist_info$stac),
    missing_species = daisie_data_colonist_info$missing_species,
    stringsAsFactors = FALSE
  )
}
