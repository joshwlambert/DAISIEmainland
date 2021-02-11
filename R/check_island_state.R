#' Checks if any changes on the mainland change the state of the island species.
#' RJCB: I think the name 'check_island_state' can be improved, as
#' it not only checks, but also updates a species status. That I cannot
#' suggest a proper name myself, means that either I fail to grasp
#' what it does and/or the function itself is a bit off. L
#'
#' @inheritParams default_params_doc
#'
#' @return List
#' RJCB: From the tests, I see it returns NULL or a matrix.
#' When does it return either?
check_island_state <- function(timeval,
                               totaltime,
                               island_spec,
                               mainland) {
  if (any(island_spec[, 4] == "I")) {
    immig_spec <- island_spec[which(island_spec[, 4] == "I"), 1]
    mainland_ex_time <- mainland[which(mainland[, 1] %in% immig_spec), 9]
    for (ex_time in mainland_ex_time) {
      if (timeval > ex_time && totaltime > ex_time) {
        island_spec[which(island_spec[, 1] == immig_spec), 4] <- "A"
        island_spec[which(island_spec[, 1] == immig_spec), 7] <-
          "mainland_extinction"
      }
    }
  }
  return(island_spec)
}
