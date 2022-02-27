#' Convert `daisie_data` into a tabular format
#' @inheritParams default_params_doc
#'
#' @return a list with these elements:
#'
#'  * `header`: general parameters of the results
#'     see \link{daisie_header_to_table}
#'  * `branching_times`: the branching times
#'  * `colonists_general`: general info of each clade
#'     see \link{daisie_data_colonist_info_to_general_table}
#'  * `colonists_branching_times`: branching times per clade
#'     see \link{daisie_data_colonist_info_to_braching_times_table}
#'  * `colonisation_times`: times that the colonisations took place
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
  colonisation_times_list <- list()

  # Due to the header, the first useful index is 2
  # 'index' is a clade index that starts counting at 2 :-/
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

    t_ct <- daisie_data_colonist_info_to_colonisation_times_table(
      daisie_data_colonist_info = daisie_data[[index]]
    )
    t_ct$clade_index <- index - 1 # Make indices start at 1, as users expect
    colonisation_times_list[[index]] <- t_ct
  }

  colonists_general <- dplyr::bind_rows(colonists_general_list)
  colonists_branching_times <- dplyr::bind_rows(colonists_branching_times_list)
  colonisation_times <- dplyr::bind_rows(
    colonisation_times_list
  )

  list(
    header = DAISIEmainland::daisie_header_to_table(
      daisie_data_header = daisie_data[[1]]),
    colonists_general = colonists_general,
    colonists_branching_times = colonists_branching_times,
    colonisation_times = colonisation_times
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
#' Convert the non-header part of a `daisie_data` into a tabular format,
#' containing, for each colonist, the branching times.
#' This excludes the time of colonisation.
#'
#' Due to the coding of DAISIE, this is not straightforward:
#'
#'  * If there are no colonisations, an empty table is produced
#'  * If there is one colonisation, the `event_times` are used
#'  * If there are two or more colonisations, the `event_times`
#'    in the `all_colonisations` list is used
#'
#' @param daisie_data_colonist_info an element of a `daisie_data`,
#' that is not the first element (the first element is of type
#' `daisie_data_header`).
#'
#' @return a table with as much rows as branching times
#'
#' @seealso use \link{daisie_data_colonist_info_to_colonisation_times_table}
#' to obtain the colonisation times of the colonists
#'
#' @examples
#' set.seed(
#'   1,
#'   kind = "Mersenne-Twister",
#'   normal.kind = "Inversion",
#'   sample.kind = "Rejection"
#' )
#' total_time <- 1
#' daisie_mainland_data <- sim_island_with_mainland(
#'   total_time = total_time,
#'   m = 10,
#'   island_pars = c(1, 1, 10, 0.1, 1),
#'   mainland_ex = 1,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "undiscovered",
#'   replicates = 1,
#'   verbose = FALSE
#' )
#' ideal_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
#'
#' # We need a non-header daisie_data
#' testthat::expect_true(length(ideal_daisie_data) > 1)
#' # Take the first non-header daisie_data
#' daisie_data_colonist_info <- ideal_daisie_data[[2]]
#' t <- daisie_data_colonist_info_to_braching_times_table(
#'   daisie_data_colonist_info = daisie_data_colonist_info
#' )
#' testthat::expect_true("colonist_index" %in% names(t))
#' testthat::expect_true("branching_times" %in% names(t))
#' # Do not include the colonisation
#' testthat::expect_false(total_time %in% t$branching_times)
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
daisie_data_colonist_info_to_braching_times_table <- function( # nolint indeed a long function name
  daisie_data_colonist_info
) {
  # Must not be a DAISIE header
  testthat::expect_false("island_age" %in% names(daisie_data_colonist_info))
  testthat::expect_false("not_present" %in% names(daisie_data_colonist_info))
  # Must be a non-header DAISIE element
  testthat::expect_true("branching_times" %in% names(daisie_data_colonist_info))
  testthat::expect_true("stac" %in% names(daisie_data_colonist_info))
  testthat::expect_true("missing_species" %in% names(daisie_data_colonist_info))

  t <- NA
  if (length(daisie_data_colonist_info$all_colonisations) == 0) {
    t <- data.frame(
      colonist_index = 1,
      branching_times = daisie_data_colonist_info$branching_times[-1],
      stringsAsFactors = FALSE
    )
  } else {
    colonisation_branching_times_list <- list()
    for (i in seq_along(daisie_data_colonist_info$all_colonisations)) {
      colonist <- daisie_data_colonist_info$all_colonisations[[i]]
      branching_times <- colonist$event_times[c(-1, -2)]

      colonisation_branching_times_list[[i]] <- data.frame(
        colonist_index = i,
        branching_times = branching_times,
        stringsAsFactors = FALSE
      )
    }
    t <- dplyr::bind_rows(colonisation_branching_times_list)
  }
  testthat::expect_true("colonist_index" %in% names(t))
  testthat::expect_true("branching_times" %in% names(t))
  t

}

#' Internal function
#'
#' Convert the non-header part of a `daisie_data` into a tabular format,
#' containing, for each colonist, the colonisation time.
#'
#' Due to the coding of DAISIE, this is not straightforward:
#'
#'  * If there are no colonisations, an empty table is produced
#'  * If there is one colonisation, the second `event_times` are used
#'    (the first `event_times` is the island age)
#'  * If there are two or more colonisations, the second `event_times`
#'    in each `all_colonisations` element is used
#'    (the first `event_times` is the island age)
#'
#' @param daisie_data_colonist_info an element of a `daisie_data`,
#' that is not the first element (the first element is of type
#' `daisie_data_header`).
#'
#' @return a table with as much rows as colonisation times
#'
#' @seealso use \link{daisie_data_colonist_info_to_braching_times_table}
#' to obtain the branching times of the colonists
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
daisie_data_colonist_info_to_colonisation_times_table <- function( # nolint indeed a long function name
  daisie_data_colonist_info
) {
  # Must not be a DAISIE header
  testthat::expect_false("island_age" %in% names(daisie_data_colonist_info))
  testthat::expect_false("not_present" %in% names(daisie_data_colonist_info))
  # Must be a non-header DAISIE element
  testthat::expect_true("branching_times" %in% names(daisie_data_colonist_info))
  testthat::expect_true("stac" %in% names(daisie_data_colonist_info))
  testthat::expect_true("missing_species" %in% names(daisie_data_colonist_info))

  t_colonisation_times <- NA
  if (length(daisie_data_colonist_info$all_colonisations) == 0) {
    testthat::expect_true(
      length(daisie_data_colonist_info$branching_times) >= 2
    )
    t_colonisation_times <- data.frame(
      colonist_index = 1,
      colonist_species_type = DAISIEmainland::stac_to_str(
        daisie_data_colonist_info$stac
      ),
      colonisation_time = daisie_data_colonist_info$branching_times[2],
      stringsAsFactors = FALSE
    )
  } else {
    colonisation_times_list <- list()
    for (i in seq_along(daisie_data_colonist_info$all_colonisations)) {
      colonist <- daisie_data_colonist_info$all_colonisations[[i]]
      event_times <- colonist$event_times
      colonisation_time <- event_times[2]

      colonisation_times_list[[i]] <- data.frame(
        colonist_index = i,
        colonist_species_type = colonist$species_type,
        colonisation_time = colonisation_time,
        stringsAsFactors = FALSE
      )
    }
    t_colonisation_times <- dplyr::bind_rows(colonisation_times_list)
  }
  testthat::expect_true("colonist_index" %in% names(t_colonisation_times))
  testthat::expect_true("colonist_species_type" %in% names(t_colonisation_times))
  testthat::expect_true("colonisation_time" %in% names(t_colonisation_times))
  t_colonisation_times
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
