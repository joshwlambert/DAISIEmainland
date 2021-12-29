#' Check if a `event_times` is valid.
#'
#' Check if a `event_times` is valid.
#' Will \link{stop} if not
#'
#' `event_times` contains:
#'
#'  * for cladogenetic species: the island age
#'    and event times of the radiation,
#'    including the stem age of the radiation
#'  * for non-endemic, non-endemic_MaxAge and
#'    Endemic anagenetic species:
#'    island age and stem age of the population/species
#'
#' A valid `event_times` ...
#'
#'  * is numeric
#'  * has only finite values
#'  * has at least 1 element (which equals the island age)
#'  * has only positive non-zero values (although zero is accepted, to prevent
#'    false negatives by rounding off errors)
#'  * has a descending order
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @examples
#' check_event_times(1.0)
#' check_event_times(c(1.0, 0.25))
#' @author Richèl J.C. Bilderbeek
#' @export
check_event_times <- function(event_times) {
  DAISIEmainland::check_branching_times(event_times)

  # As function is mainly called for side effect, see
  # See https://style.tidyverse.org/functions.html#return
  invisible(event_times)
}
