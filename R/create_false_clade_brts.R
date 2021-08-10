#' Creates a vector of branching times when empirical data is a clade but
#' the ideal data is multiple colonisations
#'
#' @inheritParams default_params_doc
#'
#' @return numeric vector
#' @keywords internal
create_false_clade_brts <- function(total_time,
                                    anc_branch_t_bp,
                                    subset_island) {
  if (all(is.na(subset_island[, "branch_t_bp"]))) {
    false_brts <- subset_island[, "col_t_bp"]
    false_brts <- false_brts[-length(false_brts)]
  } else {
    false_brts <- subset_island[, "col_t_bp"]
    false_brts <- false_brts[-length(false_brts)]
    false_brts <- unique(false_brts)
    false_brts <- sort(c(false_brts, subset_island[, "branch_t_bp"]),
                       decreasing = TRUE)
  }
  false_clade_brts <- c(total_time, anc_branch_t_bp, false_brts)
  return(false_clade_brts)
}
