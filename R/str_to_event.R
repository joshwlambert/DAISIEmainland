#' Convert a string to an event
#'
#' @param event_str the event as a string
#'
#' @return the event as a string
#'
#' @seealso use \link{event_to_str} to convent an `event` to a string
#'
#' @examples
#' str_to_event("immigration")
#' str_to_event("extinction")
#' str_to_event("anagenesis")
#' str_to_event("cladogenesis")
#'
#' @keywords internal
#' @author Richel J.C. Bilderbeek
str_to_event <- function(event_str) {
  testthat::expect_equal(length(event_str), 1)
  if (event_str == "immigration") {
    return(1)
  } else if (event_str == "extinction") {
    return(2)
  } else if (event_str == "anagenesis") {
    return(3)
  } else if (event_str == "cladogenesis") {
    return(4)
  }
  stop(
    "Invalid 'event_str' with value ", event_str, " \n",
    "Tip: values can be 'immigration', 'extinction', 'anagenesis', ",
    "or 'cladogenesis'"
  )
}
