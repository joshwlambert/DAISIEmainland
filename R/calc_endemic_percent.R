#' Calculates what percentage of island species are endemic.
#'
#' @inheritParams default_params_doc
#'
#' @return A list of six numeric vectors
#' @author Joshua W. Lambert
calc_endemic_percent <- function(daisie_data) {
  testit::assert(length(daisie_data$ideal_island) ==
                   length(daisie_data$empirical_island))

  ideal_islands_species <- list()
  empirical_islands_species <- list()

  for (i in seq_along(daisie_data$ideal_islands)) {

    ideal_islands_species[[i]] <- calc_island_endemics(
      island = daisie_data$ideal_islands[[i]])
    empirical_islands_species[[i]] <- calc_island_endemics(
      island = daisie_data$empirical_islands[[i]])
  }

  ideal_endemics <- unlist(lapply(ideal_islands_species,
                                  "[[",
                                  "endemics"))
  ideal_non_endemics <- unlist(lapply(ideal_islands_species,
                                      "[[",
                                      "non_endemics"))
  empirical_endemics <- unlist(lapply(empirical_islands_species,
                                      "[[",
                                      "endemics"))
  empirical_non_endemics <- unlist(lapply(empirical_islands_species,
                                          "[[",
                                          "non_endemics"))

  ideal_endemic_percent <-
    (ideal_endemics / (ideal_endemics + ideal_non_endemics)) * 100
  empirical_endemic_percent <-
    (empirical_endemics / (empirical_endemics + empirical_non_endemics)) * 100

  endemic_percent_list <- list(
    ideal_endemic_percent = ideal_endemic_percent,
    empirical_endemic_percent = empirical_endemic_percent,
    ideal_endemics = ideal_endemics,
    ideal_non_endemics = ideal_non_endemics,
    empirical_endemics = empirical_endemics,
    empirical_non_endemics = empirical_non_endemics)

  return(endemic_percent_list)
}
