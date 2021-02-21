#' Documentation for function arguments in the DAISIEmainland package
#'
#' @param timeval Numeric defining current time of simulation.
#' @param totaltime Numeric defining the length of the simulation in time units.
#' @param gam Numeric with the per capita immigration rate.
#' @param laa Numeric with the per capita anagenesis rate.
#' @param lac Numeric with the per capita cladogenesis rate.
#' @param mu Numeric with the per capita extinction rate.
#' @param k Numeric with carrying capacity.
#' @param num_spec Numeric with the current number of species.
#' @param num_immigrants Numeric with the current number of non-endemic
#' species.
#' @param mainland_n Numeric stating the number of mainland species that
#' can colonize the island.
#' @param island_spec Matrix with current state of simulation containing number
#' of species.
#' @param mainland Data frame with state of mainland. see
#' \code{\link{create_test_mainland}}.
#' @param mainland_spec Numeric focal species on the mainland
#' @param mainland_sample_prob Numeric between zero and one determining the
#' probability of an extant mainland species being sampled.
#' @param island_replicates List that has as many elements as replicates. Each
#' element must be a list with the elements \code{island_age} and
#' \code{not_present}. ##### LOOK INTO THIS
#' @param time Numeric defining the length of the simulation in time units.
#' @param m Numeric defining the size of mainland pool.
#' @param verbose Logical, determining if progress output should be printed
#' during the simulation.
#' @param rates named list of numeric rates as returned by
#'   \code{\link{update_rates}()}.
#' @param island_pars A numeric vector containing the parameters for the island:
#'   \itemize{
#'     \item{\code{island_pars[1]}: lambda^c (cladogenesis rate)}
#'     \item{\code{island_pars[2]}: mu (extinction rate)}
#'     \item{\code{island_pars[3]}: K (carrying capacity), set K=Inf for
#'     diversity independence.}
#'     \item{\code{island_pars[4]}: gamma (immigration rate)}
#'     \item{\code{island_pars[5]}: lambda^a (anagenesis rate)}
#'     }
#' @param mainland_ext Numeric parameter for mainland extinction rate.
#' @param replicates Integer specifying number of island replicates to be
#' simulated.
#' @param possible_event Numeric defining what event will happen.
#' @param max_spec_id Current species IDs.
#' @param mainland_scenario Integer determining which mainland clade scenario
#' is produced
#'
#' @return Nothing
default_params_doc <- function(
  timeval,
  totaltime,
  gam,
  laa,
  lac,
  mu,
  k,
  num_spec,
  num_immigrants,
  mainland_n,
  island_spec,
  mainland,
  mainland_spec,
  mainland_sample_prob,
  island_replicates,
  time,
  m,
  verbose,
  rates,
  island_pars,
  mainland_ext,
  replicates,
  possible_event,
  max_spec_id,
  mainland_scenario
) {
  #Nothing
}
