#' Calculates whether colonisations from different mainland species are more
# closely related to each other than to the extant species on the mainland
#'
#' @inheritParams default_params_doc
#'
#' @return A list of numerics or numeric vector
calc_empirical_col <- function(island_spec,
                               mainland_clade) {

  # number of independent colonisations from different mainland species
  empirical_col_present <- sort(unique(island_spec[, "main_anc_id"]))
  # where are the independent colonists in the mainland clade
  mainland_spec <- which(mainland_clade[, "spec_id"] %in% empirical_col_present)
  # is there any extant descendants of the immigrant on the mainland
  branching_code <- paste0("^", mainland_clade[mainland_spec, "branch_code"])
  descending_branches <- unique(unlist(lapply(
    branching_code,
    function(x) grep(x, mainland_clade[, "branch_code"])
  )))

  extant_mainland <-
    any(mainland_clade[descending_branches, "spec_type"] != "E" &
          mainland_clade[descending_branches, "spec_type"] != "US" &
          mainland_clade[descending_branches, "spec_type"] != "UD")

  if (isTRUE(extant_mainland)) {
    empirical_col_present_list <- as.list(empirical_col_present)
  } else if (isFALSE(extant_mainland)) {
    empirical_col_present_list <- list(empirical_col_present)
  }
  return(empirical_col_present_list)

}
