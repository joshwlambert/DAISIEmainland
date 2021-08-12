#' Parameter space for the analysis of the DAISIE mainland extinction model.
#'
#' A dataset containing the parameter sets (rows) for the simulation of the
#' DAISIE mainland extinction model for the analysis of the error inferred by
#' DAISIE's maximum likelihood model.
#'
#' @format A data frame with 768 rows and 10 variables:
#' \describe{
#'   \item{total_time}{Duration of simulation (million years)}
#'   \item{m}{Number of species on the mainland}
#'   \item{island_clado}{Rate of cladogenesis on the island}
#'   \item{island_ex}{Rate of extinction on the island}
#'   \item{island_k}{Carrying capacity for each island clade}
#'   \item{island_immig}{Rate of immigration on the island}
#'   \item{island_ana}{Rate of anagenesis on the island}
#'   \item{mainland_ex}{Rate of extinction on the mainland}
#'   \item{mainland_sample_prob}{Probability of a mainland species being
#'     sampled at the end of the simulation if it is extant}
#'   \item{replicates}{Number or island replicates}
#' }
#' @usage data("param_space", package = "DAISIEmainland")
"param_space"
