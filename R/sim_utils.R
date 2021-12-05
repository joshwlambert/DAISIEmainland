# Simulation utility functions,
# ordered alphabetically by function name:
#
# * event_to_str
# * stac_to_str
# * str_to_event
# * str_to_stac

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

#' Convert a `stac` (status of colonist) to a string
#'
#' @inheritParams default_params_doc
#'
#' @return the event as a string
#'
#' @seealso use \link{str_to_stac} to convent a string to a `stac`
#'
#' @examples
#' DAISIEmainland:::stac_to_str(0)
#' DAISIEmainland:::stac_to_str(1)
#' DAISIEmainland:::stac_to_str(2)
#' DAISIEmainland:::stac_to_str(3)
#' DAISIEmainland:::stac_to_str(4)
#' DAISIEmainland:::stac_to_str(5)
#' DAISIEmainland:::stac_to_str(6)
#' @keywords internal
#' @author Richèl J.C. Bilderbeek
stac_to_str <- function(stac) {
  testthat::expect_equal(length(stac), 1)
  if (stac == 0) {
    return("[no colonization has taken place]")
  } else if (stac == 1) {
    return("Non_endemic_MaxAge")
  } else if (stac == 2) {
    return("Endemic")
  } else if (stac == 3) {
    return("Endemic&Non_Endemic")
  } else if (stac == 4) {
    return("Non_endemic")
  } else if (stac == 5) {
    return("Endemic_singleton_MaxAge")
  } else if (stac == 6) {
    return("Endemic_clade_MaxAge")
  }
  stop(
    "Invalid event with value ", stac, " \n",
    "Tip: values can be 1, 2, 3, 4, 5 or 6"
  )
}

#' Convert a string to an event
#'
#' @param event_str the event as a string
#'
#' @return the event as a string
#'
#' @seealso use \link{event_to_str} to convent an `event` to a string
#'
#' @examples
#' DAISIEmainland:::str_to_event("immigration")
#' DAISIEmainland:::str_to_event("extinction")
#' DAISIEmainland:::str_to_event("anagenesis")
#' DAISIEmainland:::str_to_event("cladogenesis")
#'
#' @keywords internal
#' @author Richèl J.C. Bilderbeek
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

#' Convert a string to a `stac`
#'
#' @param stac_str the `stac` as a string
#'
#' @return the `stac` as a string
#'
#' @seealso use \link{stac_to_str} to convent an `stac` to a string
#'
#' @examples
#' DAISIEmainland:::str_to_stac("[no colonization has taken place]")
#' DAISIEmainland:::str_to_stac("Non_endemic_MaxAge")
#' DAISIEmainland:::str_to_stac("Endemic")
#' DAISIEmainland:::str_to_stac("Endemic&Non_Endemic")
#' DAISIEmainland:::str_to_stac("Non_endemic_MaxAge")
#' DAISIEmainland:::str_to_stac("Endemic_singleton_MaxAge")
#' DAISIEmainland:::str_to_stac("Endemic_clade_MaxAge")
#'
#' @keywords internal
#' @author Richèl J.C. Bilderbeek
str_to_stac <- function(stac_str) {
  testthat::expect_equal(length(stac_str), 1)
  if (stac_str == "[no colonization has taken place]") {
    return(0)
  } else if (stac_str == "Non_endemic_MaxAge") {
    return(1)
  } else if (stac_str == "Endemic") {
    return(2)
  } else if (stac_str == "Endemic&Non_Endemic") {
    return(3)
  } else if (stac_str == "Non_endemic") {
    return(4)
  } else if (stac_str == "Endemic_singleton_MaxAge") {
    return(5)
  } else if (stac_str == "Endemic_clade_MaxAge") {
    return(6)
  }
  stop(
    "Invalid 'stac_str' with value ", stac_str, " \n",
    "Tip: values can be '[no colonization has taken place]', ",
    "'Non_endemic_MaxAge', 'Endemic', ",
    "'Endemic&Non_Endemic', 'Endemic_singleton_MaxAge' or ",
    "'Endemic_clade_MaxAge'"
  )
}
