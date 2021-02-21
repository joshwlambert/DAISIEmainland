#' Checks and updates if any changes on the mainland change the state of the
#' island species.
#'
#' @inheritParams default_params_doc
#'
#' @return Matrix or NULL
update_island_endemics <- function(
  timeval,
  totaltime,
  island_spec,
  mainland) {
  if (any(island_spec[, 4] == "I")) {
    immig_spec <- island_spec[which(island_spec[, 4] == "I"), 1]
    mainland_ex_time <- mainland[which(mainland[, 1] %in% immig_spec), 9]
    for (i in seq_along(mainland_ex_time)) {
      if (timeval > mainland_ex_time[i] && totaltime > mainland_ex_time[i]) {
        island_spec[which(island_spec[, 1] == immig_spec[i]), 4] <- "A"
        island_spec[which(island_spec[, 1] == immig_spec[i]), 7] <-
          "mainland_extinction"
      }
    }
  }
  return(island_spec)
}
