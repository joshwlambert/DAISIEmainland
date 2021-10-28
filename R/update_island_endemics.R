#' Checks and updates if any changes on the mainland change the state of the
#' island species.
#'
#' @inheritParams default_params_doc
#'
#' @return Data frame of island species
#' @author Joshua W. Lambert
update_island_endemics <- function(timeval,
                                   total_time,
                                   island_spec,
                                   mainland_clade) {

  testit::assert(is.numeric(timeval))
  testit::assert(is.numeric(total_time))
  testit::assert(is.data.frame(island_spec))
  testit::assert(ncol(island_spec) == 7)
  testit::assert(is.data.frame(mainland_clade))
  testit::assert(ncol(mainland_clade) == 7)

  if (any(island_spec[, "spec_type"] == "I")) {
    immig_spec <-
      island_spec[which(island_spec[, "spec_type"] == "I"), "spec_id"]
    mainland_ex_time <-
      mainland_clade[which(mainland_clade[, "spec_id"] %in% immig_spec),
                     "spec_ex_t"]
    mainland_spec_type <-
      mainland_clade[which(mainland_clade[, "spec_id"] %in% immig_spec),
                     "spec_type"]
    for (i in seq_along(mainland_ex_time)) {
      if (timeval > mainland_ex_time[i] && total_time > mainland_ex_time[i]) {
        if (mainland_spec_type[i] != "US") {
          island_spec[island_spec[, "spec_id"] == immig_spec[i], "spec_type"] <-
            "A"
          island_spec[island_spec[, "spec_id"] == immig_spec[i], "ana_origin"] <-
            "mainland_extinction"
        }
      }
    }
  }
  return(island_spec)
}
