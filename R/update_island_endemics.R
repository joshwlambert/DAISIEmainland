#' Checks and updates if any changes on the mainland change the state of the
#' island species.
#'
#' @inheritParams default_params_doc
#'
#' @return Matrix or NULL
update_island_endemics <- function(
  timeval,
  total_time,
  island_spec,
  mainland_clade) {
  if (any(island_spec[, 4] == "I")) {
    immig_spec <- island_spec[which(island_spec[, 4] == "I"), 1]
    mainland_ex_time <- mainland_clade[which(mainland_clade[, 1] %in% immig_spec), 9]
    for (i in seq_along(mainland_ex_time)) {
      if (timeval > mainland_ex_time[i] && total_time > mainland_ex_time[i]) {
        island_spec[which(island_spec[, 1] == immig_spec[i]), 4] <- "A"
        island_spec[which(island_spec[, 1] == immig_spec[i]), 7] <-
          "mainland_extinction"
      }
    }
  }
  return(island_spec)
}
