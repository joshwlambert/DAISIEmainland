#' Convert an event to a string
#'
#' @inheritParams default_params_doc
#'
#' @return the event as a string
#'
#' @seealso use \link{str_to_event} to convent a string to an `event`
#'
#' @examples
#' DAISIEmainland:::event_to_str(1)
#' DAISIEmainland:::event_to_str(2)
#' DAISIEmainland:::event_to_str(3)
#' DAISIEmainland:::event_to_str(4)
#'
#' @keywords internal
#' @author Richèl J.C. Bilderbeek
event_to_str <- function(possible_event) {
  testthat::expect_equal(length(possible_event), 1)
  if (possible_event == 1) {
    return("immigration")
  } else if (possible_event == 2) {
    return("extinction")
  } else if (possible_event == 3) {
    return("anagenesis")
  } else if (possible_event == 4) {
    return("cladogenesis")
  }
  stop(
    "Invalid event with value ", possible_event, " \n",
    "Tip: values can be 1, 2, 3, or 4"
  )
}
