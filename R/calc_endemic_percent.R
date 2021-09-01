#' Calculates what percentage of island species are endemic.
#'
#' @inheritParams default_params_doc
#'
#' @return A list of six numeric vectors
#' @author Joshua W. Lambert
calc_endemic_percent <- function(daisie_data) {

  ideal_endemic_percent_vec <- c()
  empirical_endemic_percent_vec <- c()
  ideal_endemics_vec <- c()
  ideal_non_endemics_vec <- c()
  empirical_endemics_vec <- c()
  empirical_non_endemics_vec <- c()

  testit::assert(length(daisie_data$ideal_island) ==
                   length(daisie_data$empirical_island))

  for (i in seq_along(daisie_data$ideal_islands)) {

    ideal_endemics <- 0
    ideal_non_endemics <- 0
    empirical_endemics <- 0
    empirical_non_endemics <- 0

    for (j in 2:length(daisie_data$ideal_islands[[i]])) {
      ideal_stac <- daisie_data$ideal_islands[[i]][[j]]$stac
      if (ideal_stac == 1 || ideal_stac == 2 || ideal_stac == 4 ||
          ideal_stac == 5 || ideal_stac == 6) {
        if (ideal_stac == 2 || ideal_stac == 5 || ideal_stac == 6) {
          ideal_endemics <- ideal_endemics + 1
        } else if (ideal_stac == 1 || ideal_stac == 4) {
          ideal_non_endemics <- ideal_non_endemics + 1
        } else {
          stop("Incorrect stac in ideal data")
        }
      } else {
        for (k in seq_along(daisie_data$ideal_islands[[i]][[j]]$all_colonisations)) {
          species_type <- daisie_data$ideal_islands[[i]][[j]]$all_colonisations[[k]]$species_type
          if (species_type == "C" || species_type == "A") {
            ideal_endemics <- ideal_endemics + 1
          } else if (species_type == "I") {
            ideal_non_endemics <- ideal_non_endemics + 1
          } else {
            stop("Incorrect species_type in stac 3 ideal data")
          }
        }
      }
    }

    for (j in 2:length(daisie_data$empirical_islands[[i]])) {
      empirical_stac <- daisie_data$empirical_islands[[i]][[j]]$stac
      if (empirical_stac == 1 || empirical_stac == 2 || empirical_stac == 4 ||
          empirical_stac == 5 || empirical_stac == 6) {
        if (empirical_stac == 2 || empirical_stac == 5 || empirical_stac == 6) {
          empirical_endemics <- empirical_endemics + 1
        } else if (empirical_stac == 1 || empirical_stac == 4) {
          empirical_non_endemics <- empirical_non_endemics + 1
        } else {
          stop("Incorrect stac in empirical data")
        }
      } else {
        for (k in seq_along(daisie_data$empirical_islands[[i]][[k]]$all_colonisations)) {
          species_type <- daisie_data$empirical_islands[[i]][[j]]$all_colonistations[[k]]$species_type
          if (species_type == "C" || species_type == "A") {
            empirical_endemics <- empirical_endemics + 1
          } else if (species_type == "I") {
            empirical_non_endemics <- empirical_non_endemics + 1
          } else {
            stop("Incorrect species_type in stac 3 empirical data")
          }
        }
      }
    }

    # calculate delta colonisation through time
    testit::assert(ideal_endemics >= 0)
    testit::assert(ideal_non_endemics >= 0)
    testit::assert(empirical_endemics >= 0)
    testit::assert(empirical_non_endemics >= 0)

    ideal_endemic_percent <-
      (ideal_endemics / (ideal_endemics + ideal_non_endemics)) * 100
    empirical_endemic_percent <-
      (empirical_endemics / (empirical_endemics + empirical_non_endemics)) * 100


      ideal_endemic_percent_vec <- c(ideal_endemic_percent_vec,
                                     ideal_endemic_percent)
      empirical_endemic_percent_vec <- c(empirical_endemic_percent_vec,
                                         empirical_endemic_percent)
      ideal_endemics_vec <- c(ideal_endemics_vec, ideal_endemics)
      ideal_non_endemics_vec <- c(ideal_non_endemics_vec, ideal_non_endemics)
      empirical_endemics_vec <- c(empirical_endemics_vec, empirical_endemics)
      empirical_non_endemics_vec <- c(empirical_non_endemics_vec,
                                      empirical_non_endemics)
  }

  endemic_percent_list <- list(
    ideal_endemic_percent = ideal_endemic_percent_vec,
    empirical_endemic_percent = empirical_endemic_percent_vec,
    ideal_endemics = ideal_endemics_vec,
    ideal_non_endemics = ideal_non_endemics_vec,
    empirical_endemics = empirical_endemics_vec,
    empirical_non_endemics = empirical_non_endemics_vec)

  return(endemic_percent_list)
}
