#' Extract all colonisations from an island history
#' and put these in a table
#' @inheritParams default_params_doc
#'
#' @return a \link{list} with elements:
#'   * `t`: a table
#'   * `empirical_island$all_colonisations`: a list with all colonizations
#'     in the empirical data
#'   * `ideal_island$all_colonisations`: a list with all colonizations
#'     in the ideal data
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
all_colonisations_to_table <- function(ideal_or_empirical_island) {

  # Collect the 'ideal_or_empirical_island[[i]]$all_colonisations's
  # in one list without adding a dependency on purrr
  all_colonisations_list <- list()
  for (i in seq_along(ideal_or_empirical_island)) {
    if (is.null(ideal_or_empirical_island[[i]]$all_colonisations)) next
    all_colonisations_list[[i]] <- ideal_or_empirical_island[[i]]$all_colonisations
    all_colonisations_list[[i]]$clade_id <- i
    ideal_or_empirical_island[[i]]$all_colonisations <- NULL
  }
  all_colonisations_list
  tibble::tibble(

  )
}
