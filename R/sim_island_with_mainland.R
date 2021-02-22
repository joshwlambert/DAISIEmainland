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
#' \code{ideal_islands} and \code{empirical_islands} which corresponds to
#' the ideal and empirical data sets produce in each simulation. Within each
#' \code{ideal_islands} and \code{empirical_islands} each element is an
#' individual replicate. The first element of each replicate is composed of
#' island information containing:
#' \itemize{
#'   \item{\code{$island_age}: A numeric with the island age.}
#'   \item{\code{$not_present}: the number of mainland lineages that are not
#'     present on the island.}
#' }
#' The subsequent elements of the list pertaining to each replcate contain
#' information on a single colonist lineage on the island and have 4 components:
#' \itemize{
#'   \item{\code{$branching_times}: island age and stem age of the
#'     population/species in the case of Non-endemic, Non-endemic_MaxAge and
#'     Endemic anagenetic species.
#'
#'     For cladogenetic species these should
#'     be island age and branching times of the radiation including the
#'     stem age of the radiation.}
#'   \item{\code{$stac}: An integer ranging from 1 to 6
#'   indicating the status of the colonist:}
#'   \enumerate{
#'     \item Non_endemic_MaxAge
#'     \item Endemic
#'     \item Endemic&Non_Endemic
#'     \item Non_endemic_MaxAge
#'     \item Endemic_singleton_MaxAge
#'     \item Endemic_clade_MaxAge
#' }
#' \item{\code{$missing_species}: number of island species that were
#' not sampled for particular clade (only applicable for endemic clades)}
#' \item{\code{$type_1or2}: whether the colonist belongs to type 1 or type 2}
#' }
#' @author Joshua Lambert
#' @examples
#' ## Simulate 2 islands (replicates) for 1 million years, with a mainland
#' ## extinction rate of 1 (SpMy^-1). Pool size 100.
#'
#' set.seed(1)
#' island <- sim_island_with_mainland(
#'   time = 1,
#'   m = 100,
#'   island_pars = c(1, 1, 10, 0.1, 1),
#'   mainland_ext = 1,
#'   mainland_sample_prob = 1,
#'   replicates = 2,
#'   verbose = FALSE
#' )
#'
#' @export sim_island_with_mainland
sim_island_with_mainland <- function(
  time,
  m,
  island_pars,
  mainland_ext,
  mainland_sample_prob,
  replicates,
  verbose = FALSE
) {
  testit::assert(is.numeric(time))
  testit::assert(time > 0)
  testit::assert(is.numeric(m))
  testit::assert(m > 1)
  testit::assert(is.numeric(island_pars))
  testit::assert(length(island_pars) == 5)
  testit::assert(island_pars[4] > 0)
  testit::assert(is.numeric(mainland_ext))
  testit::assert(mainland_ext >= 0)
  testit::assert(is.numeric(mainland_sample_prob))
  testit::assert(mainland_sample_prob >= 0 && mainland_sample_prob <= 1)
  testit::assert(is.numeric(replicates))
  testit::assert(replicates >= 1)
  testit::assert(is.logical(verbose))

  total_time <- time
  island_replicates <- list()

  mainland_replicates <- list()
  for (rep in 1:replicates) {
    if (verbose) {
      print(paste0("Island replicate ", rep))
    }
    island_replicates[[rep]] <- list()
    mainland_replicates[[rep]] <- list()
    full_list <- list()
    mainland_replicates[[rep]] <- sim_mainland(
      time = time,
      m = m,
      mainland_ext = mainland_ext)
    for (m_spec in seq_along(mainland_replicates[[rep]])) {
      full_list[[m_spec]] <- sim_island(
        time = total_time,
        m = m,
        island_pars = island_pars,
        mainland_clade = mainland_replicates[[rep]][[m_spec]],
        mainland_sample_prob = mainland_sample_prob
      )
    }
    island_replicates[[rep]] <- full_list
  }
  island_replicates <- format_to_daisie_data(
    island_replicates = island_replicates,
    time = total_time,
    m = m)
  return(island_replicates)
}
