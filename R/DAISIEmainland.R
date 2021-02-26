#' \code{DAISIEmainland}: A package to simulate phylogenetic data on an island
#' with the presence of mainland speciation and extinction to test the
#' \code{DAISIE} model of island biogeography.
#'
#' \code{DAISIEmainland} simulates a two data sets for every island: 1) the
#' ideal data set has the full information, 2) the empirical data set has
#' the data as would be seen by the empirical collecting phylogenetic data
#' in the presence of mainland dynamics.
#'
#' @examples
#' \dontrun{
#' # simulate data for 1000 islands
#' replicates <- 1000
#' island <- sim_island_with_mainland(
#' time = 1,
#' m = 100,
#' island_pars = c(1, 1, 10, 0.1, 1),
#' mainland_ex = 1,
#' mainland_sample_prob = 1,
#' replicates = replicates,
#' verbose = FALSE
#' )
#'}
#'
#' @seealso
#' \code{DAISIEmainland} works in association with the
#' package \code{DAISIE}(github.com/rsetienne/DAISIE)
#' @author Joshua W. Lambert
#' @docType package
#' @name DAISIEmainland
NULL
