#' Updates state of island given sampled event for a constant rate case.
#'
#' Makes the event happen by updating island species matrix and species IDs.
#' What event happens is determined by the sampling in the algorithm.
#'
#' @inheritParams default_params_doc
#'
#' @return The updated state of the system, which is a list with the
#' `island_tbl` matrix and an integer `max_spec_id` with the most
#' recent ID of species.
#'
#' @keywords internal
#' @author Joshua W. Lambert
update_state <- function(timeval,
                         total_time,
                         possible_event,
                         max_spec_id,
                         mainland_spec,
                         island_tbl) {
  testit::assert(is.numeric(timeval))
  testit::assert(is.numeric(total_time))
  testit::assert(is.numeric(possible_event))
  testit::assert(possible_event >= 1)
  testit::assert(possible_event <= 4)
  testit::assert(is.numeric(max_spec_id))
  testit::assert(is.numeric(mainland_spec))
  testit::assert(is.data.frame(island_tbl))
  testit::assert(ncol(island_tbl) == 7)

  # IMMIGRATION
  if (possible_event == 1) {
    island_tbl <- immig_event(
      timeval = timeval,
      island_tbl = island_tbl,
      mainland_spec = mainland_spec
    )
  }

  # EXTINCTION
  if (possible_event == 2) {
    island_tbl <- ext_event(
      island_tbl = island_tbl
    )
  }

  # ANAGENESIS
  if (possible_event == 3) {
    updated_state <- ana_event(
      island_tbl = island_tbl,
      max_spec_id = max_spec_id
    )
    island_tbl <- updated_state$island_tbl
    max_spec_id <- updated_state$max_spec_id
  }

  # CLADOGENESIS
  if (possible_event == 4) {
    updated_state <- clado_event(
      timeval = timeval,
      island_tbl = island_tbl,
      max_spec_id = max_spec_id
    )
    island_tbl <- updated_state$island_tbl
    max_spec_id <- updated_state$max_spec_id
  }
  return(list(
    island_tbl = island_tbl,
    max_spec_id = max_spec_id
  ))
}
