#' Calculates the branching time of the common ancestor of two species on the
#' mainland.
#'
#' @inheritParams default_params_doc
#'
#' @return Numeric
#' @keywords internal
common_ancestor_time <- function(total_time,
                                 mainland_spec,
                                 mainland_clade) {
  focal_spec <- mainland_clade[mainland_spec, "branch_code"]
  sister_spec <- mainland_clade[which(mainland_clade[, "spec_type"] != "E"),
                                "branch_code"]
  focal_spec_split <- strsplit(focal_spec, "")[[1]]
  sister_spec_split <- strsplit(sister_spec, "")
  common_ancestor_split <- vector("list", length(sister_spec_split))
  common_ancestor <- vector("list", length(sister_spec_split))
  common_ancestor_brts <- vector("list", length(sister_spec_split))
  for (i in seq_along(sister_spec_split)) {
    for (j in seq_along(focal_spec_split)) {
      if (focal_spec_split[[j]] == sister_spec_split[[i]][[j]]) {
        common_ancestor_split[[i]][j] <- focal_spec_split[[j]]
      } else {
        break
      }
    }
    common_ancestor[[i]] <- paste0(common_ancestor_split[[i]], collapse = "")
    common_ancestor_brts[[i]] <-
      mainland_clade[which(
        mainland_clade[, "branch_code"] == common_ancestor[[i]]), "spec_ex_t"]
  }
  common_ancestor_brts <- max(unlist(common_ancestor_brts))
  # set common ancestor branching time to time before the present
  common_ancestor_brts <- total_time - common_ancestor_brts
  return(common_ancestor_brts)
}
