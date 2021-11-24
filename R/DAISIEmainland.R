#' `DAISIEmainland`: A package to simulate phylogenetic data on an island
#' with the presence of mainland speciation and extinction to test the
#' `DAISIE` model of island biogeography.
#'
#' `DAISIEmainland` simulates a two data sets for every island: 1) the
#' ideal data set has the full information, 2) the empirical data set has
#' the data as would be seen by the empirical collecting phylogenetic data
#' in the presence of mainland dynamics.
#'
#' @examples
#' # simulate data for 1 island (replicate) with mainland extinction
#' island <- sim_island_with_mainland(
#' total_time = 1,
#' m = 100,
#' island_pars = c(1, 1, 10, 0.1, 1),
#' mainland_ex = 0.1,
#' mainland_sample_prob = 1,
#' mainland_sample_type = "complete",
#' replicates = 1,
#' verbose = FALSE
#' )
#' # simulate data for 1 island (replicate) with incomplete sampling of known
#' # mainland species
#' island <- sim_island_with_mainland(
#' total_time = 1,
#' m = 100,
#' island_pars = c(1, 1, 10, 0.1, 1),
#' mainland_ex = 0,
#' mainland_sample_prob = 0.8,
#' mainland_sample_type = "unsampled",
#' replicates = 1,
#' verbose = FALSE
#' )
#'
#' @seealso
#' `DAISIEmainland` works in association with the
#' package `DAISIE`(github.com/rsetienne/DAISIE)
#' @author Joshua W. Lambert
#' @docType package
#' @name DAISIEmainland
NULL
