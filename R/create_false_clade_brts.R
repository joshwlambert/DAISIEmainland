#' Creates a vector of branching times when empirical data is a clade but
#' the ideal data is multiple colonisations
#'
#' @inheritParams default_params_doc
#'
#' @return numeric vector
#' @keywords internal
#' @author Joshua W. Lambert
create_false_clade_brts <- function(total_time,
                                    anc_branch_t_bp,
                                    subset_island) {
  if (all(is.na(subset_island[, "branch_t_bp"]))) {
    false_brts <- unique(sort(subset_island[, "col_t_bp"], decreasing = TRUE))
    false_brts <- false_brts[-length(false_brts)]
  } else {
    false_brts <- unique(sort(subset_island[, "col_t_bp"], decreasing = TRUE))
    second_col <- false_brts[length(false_brts)]
    false_brts <- unique(sort(c(false_brts, subset_island[, "branch_t_bp"]),
                       decreasing = TRUE))
    false_brts <- false_brts[-which(false_brts == second_col)]
  }
  false_clade_brts <- c(total_time, anc_branch_t_bp, false_brts)
  return(false_clade_brts)
}
