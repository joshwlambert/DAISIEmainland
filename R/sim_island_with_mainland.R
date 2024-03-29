#' Simulate oceanic islands with mainland extinction given
#' parameters under time-constant rates.
#'
#' @description This function simulates islands with given cladogenesis,
#' extinction, carrying capacity, immigration and anagenesis parameters, and
#' simulates the mainland given an extinction rate parameter, all of which are
#' modelled as time-constant parameters.
#'
#' @inheritParams default_params_doc
#'
#' @return A list. The highest level of the list has two elements called
#' `ideal_islands` and `empirical_islands` which corresponds to
#' the ideal and empirical data sets produce in each simulation. Within each
#' `ideal_islands` and `empirical_islands` each element is an
#' individual replicate. The first element of each replicate is composed of
#' island information containing:
#' \describe{
#'   \item{`$island_age`}{A numeric with the island age.}
#'   \item{`$not_present`}{the number of mainland lineages that are not
#'     present on the island.}
#' }
#' The subsequent elements of the list pertaining to each replicate contain
#' information on a single colonist lineage on the island and have 3 components:
#' \describe{
#'   \item{`$branching_times`}{island age and stem age of the
#'     population/species in the case of Non-endemic, Non-endemic_MaxAge and
#'     Endemic anagenetic species.
#'     For cladogenetic species these should
#'     be island age and branching times of the radiation including the
#'     stem age of the radiation.}
#'   \item{`$stac`}{An integer ranging from 1 to 6
#'   indicating the status of the colonist:
#'   \enumerate{
#'     \item Non_endemic_MaxAge
#'     \item Endemic
#'     \item Endemic&Non_Endemic
#'     \item Non_endemic_MaxAge
#'     \item Endemic_singleton_MaxAge
#'     \item Endemic_clade_MaxAge
#' }}
#' \item{`missing_species`}{number of island species that were
#' not sampled for particular clade (only applicable for endemic clades)}
#' }
#' @author Joshua W. Lambert
#' @examples
#' ## Simulate 2 islands (replicates) for 1 million years, with a mainland
#' ## extinction rate of 1 (SpMy^-1). Pool size 100.
#'
#' set.seed(
#'   1,
#'   kind = "Mersenne-Twister",
#'   normal.kind = "Inversion",
#'   sample.kind = "Rejection"
#' )
#' island <- sim_island_with_mainland(
#'   total_time = 1,
#'   m = 100,
#'   island_pars = c(1, 1, 10, 0.1, 1),
#'   mainland_ex = 1,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "undiscovered",
#'   replicates = 2,
#'   verbose = FALSE
#' )
#' @export
sim_island_with_mainland <- function(total_time,                                # nolint, unavoidable cyclomatic complexity
                                     m,
                                     island_pars,
                                     mainland_ex,
                                     mainland_sample_prob,
                                     mainland_sample_type,
                                     replicates,
                                     verbose = FALSE) {
  testit::assert(is.numeric(total_time))
  testit::assert(total_time >= 0)
  testit::assert(is.numeric(m))
  testit::assert(m >= 1)
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
  testit::assert(is.numeric(replicates))
  testit::assert(replicates >= 1)
  testit::assert(is.logical(verbose))

  if (m == 1 && mainland_ex > 0) {
    stop("Simulating with mainland extinction requires more than one clade")
  }
  if (mainland_sample_type == "complete" && mainland_sample_prob < 1.0) {
    stop("Mainland sampling probability less than 1.0 requires a sampling type")
  }

  multi_daisie_data <- list()
  multi_mainland_clade <- list()

  for (rep in seq_len(replicates)) {
    if (verbose) {
      message("Island replicate ", rep)
    }
    multi_daisie_data[[rep]] <- list()
    multi_mainland_clade[[rep]] <- list()
    island_tbl_list <- list()
    daisie_data_list <- list()
    multi_mainland_clade[[rep]] <- sim_mainland(
      total_time = total_time,
      m = m,
      mainland_ex = mainland_ex
    )
    for (mainland_clade in seq_along(multi_mainland_clade[[rep]])) {
      island_tbl_list[[mainland_clade]] <- sim_island(
        total_time = total_time,
        island_pars = island_pars,
        mainland_clade = multi_mainland_clade[[rep]][[mainland_clade]],
        mainland_sample_prob = mainland_sample_prob,
        mainland_sample_type = mainland_sample_type
      )

      daisie_data_list[[mainland_clade]] <- create_daisie_data(
        total_time = total_time,
        island_tbl = island_tbl_list[[mainland_clade]],
        mainland_clade = multi_mainland_clade[[rep]][[mainland_clade]],
        mainland_sample_prob = mainland_sample_prob,
        mainland_sample_type = mainland_sample_type
      )
    }

    multi_daisie_data[[rep]] <- daisie_data_list
  }

  daisie_mainland_data <- group_multi_daisie_data(
    multi_daisie_data = multi_daisie_data,
    total_time = total_time,
    m = m
  )

  ideal_multi_daisie_data <- add_metadata_to_daisie_data(
    multi_daisie_data = daisie_mainland_data$ideal_multi_daisie_data,
    total_time = total_time,
    m = m
  )

  empirical_multi_daisie_data <- add_metadata_to_daisie_data(
    multi_daisie_data = daisie_mainland_data$empirical_multi_daisie_data,
    total_time = total_time,
    m = m
  )

  daisie_mainland_data <- list(
    ideal_multi_daisie_data = ideal_multi_daisie_data,
    empirical_multi_daisie_data = empirical_multi_daisie_data
  )

  return(daisie_mainland_data)
}
