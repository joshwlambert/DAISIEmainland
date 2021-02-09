#' Simulate oceanic islands with mainland extinction given
#' parameters under time-constant rates.
#'
#' @description
#' This function simulates islands with given cladogenesis,
#' extinction, Kprime, immigration and anagenesis parameters, and simulates
#' the mainland given an extinction rate parameter, all of which are modelled as
#' time-constant parameters.
#'
#' @inheritParams default_params_doc
#'
#' @return
#' A list. The highest level of the least corresponds to each individual
#' replicate. The first element of each replicate is composed of island
#' information containing:
#' \itemize{
#'   \item{\code{$island_age}: A numeric with the island age.}
#'   \item{\code{$not_present}: the number of mainland lineages that are not
#'     present on the island.}
#'   \item{\code{$stt_all}: STT table for all species on the island
#'     (nI - number of non-endemic species; nA - number of anagenetic species,
#'     nC - number of cladogenetic species, present - number of independent
#'     colonisations present)}
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
#'   \item{\code{$stac}: An integer ranging from 1 to 4
#'   indicating the status of the colonist:}
#'   \enumerate{
#'     \item Non_endemic_MaxAge
#'     \item Endemic
#'     \item Endemic&Non_Endemic
#'     \item Non_endemic_MaxAge
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
#' island <- sim_island_mainland(
#'   time = 1,
#'   m = 100,
#'   island_pars = c(1, 1, 10, 0.1, 1),
#'   mainland_ext = 1,
#'   mainland_sample_prob = 1,
#'   replicates = 2,
#'   verbose = FALSE
#' )
#'
#' @export sim_island_mainland
sim_island_mainland <- function(
  time,
  m,
  island_pars,
  mainland_ext,
  mainland_sample_prob,
  replicates,
  verbose = TRUE
) {
  testit::assert(is.numeric(time))
  testit::assert(time > 0)
  testit::assert(is.numeric(m))
  testit::assert(m > 1)
  testit::assert(is.numeric(island_pars))
  testit::assert(length(island_pars) == 5)
  testit::assert(is.numeric(mainland_ext))
  testit::assert(length(mainland_ext) == 1)
  testit::assert(is.numeric(replicates))
  testit::assert(island_pars[4] > 0)

  totaltime <- time
  island_replicates <- list()
  mainland_replicates <- list()
  for (rep in 1:replicates) {
    island_replicates[[rep]] <- list()
    mainland_replicates[[rep]] <- list()
    full_list <- list()
    mainland_replicates[[rep]] <- sim_mainland(
      time = time,
      m = m,
      mainland_ext = mainland_ext)
    for (m_spec in seq_along(mainland_replicates[[rep]])) {
      full_list[[m_spec]] <- sim_island(
        time = totaltime,
        m = m,
        island_pars = island_pars,
        mainland = mainland_replicates[[rep]][[m_spec]],
        mainland_sample_prob = mainland_sample_prob
      )
    }
    island_replicates[[rep]] <- full_list
    if (verbose == TRUE) {
      print(paste("Island replicate ", rep, sep = ""))
    }
  }
  island_replicates <- format_data(
    island_replicates = island_replicates,
    time = totaltime,
    m = m,
    verbose = verbose
  )
  return(island_replicates)
}
