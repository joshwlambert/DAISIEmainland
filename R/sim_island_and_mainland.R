#' Simulate both the island and mainland,
#' a version of \link{sim_island_with_main} used in debugging, that
#'  * returns both the island and the mainland,
#'    to verify the correctness of the simulation
#'  * has no 1,
#'    to simplify reading
#'
#' @inheritParams default_params_doc
#'
#' @return a list with elements
#'  * `mainland`
#'  * `ideal_island`
#'  * `empirical_island`
#'
#' See \link{sim_island_with_mainland} for the superior documentation
#' @examples
#' island <- sim_island_and_mainland(
#'   total_time = 1,
#'   m = 1, # Number of mainland clades
#'   island_pars = c(1, 1, 10, 0.1, 1),
#'   mainland_ex = 1,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "undiscovered"
#' )
#' @author original function by Joshua W. Lambert,
#' after which Richèl J.C. Bilderbeek adapted it to this debug function by
#' @export
sim_island_and_mainland <- function(total_time,
                                     m,
                                     island_pars,
                                     mainland_ex,
                                     mainland_sample_prob,
                                     mainland_sample_type,
                                     verbose = FALSE) {
  testit::assert(is.numeric(total_time))
  testit::assert(total_time > 0)
  testit::assert(is.numeric(m))
  testit::assert(m > 1)
  testit::assert(is.numeric(island_pars))
  testit::assert(length(island_pars) == 5)
  testit::assert(island_pars[4] > 0)
  testit::assert(is.numeric(mainland_ex))
  testit::assert(mainland_ex >= 0)
  testit::assert(is.numeric(mainland_sample_prob))
  testit::assert(mainland_sample_prob >= 0 && mainland_sample_prob <= 1)
  testit::assert(mainland_sample_type == "unsampled" ||
                   mainland_sample_type == "undiscovered" ||
                   mainland_sample_type == "complete")
  testit::assert(is.logical(verbose))

  if (mainland_sample_type == "complete" && mainland_sample_prob < 1.0) {
    stop("Mainland sampling probability less than 1.0 requires a sampling type")
  }

  number_of_mainland_clades <- m

  mainland <- sim_mainland(
    total_time = total_time,
    m = number_of_mainland_clades,
    mainland_ex = mainland_ex
  )


  island <- list()
  full_list <- list()
  for (mainland_clade in seq_along(mainland)) {
    message("mainland_clade: ", mainland_clade)
    full_list[[mainland_clade]] <- sim_island(
      total_time = total_time,
      island_pars = island_pars,
      mainland_clade = mainland[[mainland_clade]],
      mainland_sample_prob = mainland_sample_prob,
      mainland_sample_type = mainland_sample_type
    )
  }

  island <- full_list

  island_1 <- format_to_daisie_data(
    island_1 = island_1,
    total_time = total_time,
    m = m)

  return(island_1)
}
