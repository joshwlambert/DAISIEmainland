# Functions used for testing
#
# *
#

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
  n <- rep(0, length(mainland))
  for (i in seq_along(mainland)) {
    n[i] <- sum(mainland[[i]]$spec_ex_t == 1.0)
  }
  #
  # Prove it is the same as using purrr:
  #
  # testthat::expect_equal(n, purrr::map_dbl(mainland, function(x) { return(sum(x$spec_ex_t == 1.0)) } )) # nolint indeed, this code :-)

  n
}
