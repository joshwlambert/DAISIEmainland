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
  if (any(island_spec[, "spec_type"] == "I")) {
    immig_spec <- island_spec[which(island_spec[, "spec_type"] == "I"), "spec_id"]
    mainland_ex_time <- mainland_clade[which(mainland_clade[, "spec_id"] %in% immig_spec), "spec_ex_t"]
    for (i in seq_along(mainland_ex_time)) {
      if (timeval > mainland_ex_time[i] && total_time > mainland_ex_time[i]) {
        island_spec[which(island_spec[, "spec_id"] == immig_spec[i]), "spec_type"] <- "A"
        island_spec[which(island_spec[, "spec_id"] == immig_spec[i]), "ana_origin"] <-
          "mainland_extinction"
      }
    }
  }
  return(island_spec)
}
