# Functions used for testing
#
# *
#


#' Collect the `stac`s of an `ideal_island`
#' @inheritParams default_params_doc
#' @return a numeric vector of `stac`s
#' @author Richèl J.C. Bilderbeek
#' @export
collect_ideal_island_stacs <- function(ideal_island) {
  # Same as using purrr:
  #
  # purrr::map_dbl(ideal_island, function(x) x$stac) # nolint indeed, this code :-)
  #
  #
  # We use this handcrafted function to avoid adding a dependency
  #
  stacs <- rep(-1, length(ideal_island))
  for (i in seq_along(ideal_island)) {
    stacs[i] <- ideal_island[[i]]$stac
  }
  # Prove it is indeed the same as using purrr:
  #
  testthat::expect_equal(stacs, purrr::map_dbl(ideal_island, function(x) x$stac)) # nolint indeed, this code :-)
  stacs
}

#' Collect the `stac`s of an `empirical_island`
#' @inheritParams default_params_doc
#' @return a numeric vector of `stac`s
#' @author Richèl J.C. Bilderbeek
#' @export
collect_empirical_island_stacs <- function(empirical_island) {
  # As there is no check on the data type,
  # we can shamelessly do this:
  DAISIEmainland::collect_ideal_island_stacs(empirical_island)
}

#' Count the number of extant mainland species
#' @inheritParams default_params_doc
#' @return the number of extant mainland species
#' @author Richèl J.C. Bilderbeek
#' @export
count_extant_mainland_species <- function(mainland) {
  #
  # Same as using purrr:
  #
  # purrr::map_dbl(mainland, function(x) { return(sum(x$spec_ex_t == 1.0)) } ) # nolint indeed, this is code :-)
  #
  # We use this handcrafted function to avoid adding a dependency
  #
  n <- 0
  for (clade in mainland) {
    n <- n + sum(clade$spec_ex_t == 1.0)
  }
  #
  # Prove it is the same as using purrr:
  #
  testthat::expect_equal(n, purrr::map_dbl(mainland, function(x) { return(sum(x$spec_ex_t == 1.0)) } )) # nolint indeed, this code :-)

  n
}
