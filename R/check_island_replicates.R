#' Check if an `island_replicates` is valid.
#'
#' Check if an `island_replicates` is valid.
#' Will \link{stop} if not.
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
check_island_replicates <- function(island_replicates) {
  testthat::expect_true(is.list(island_replicates))
  testthat::expect_true(length(island_replicates) >= 1)

  for (island_replicate_index in seq_along(island_replicates)) {
    # Cannot use 'island' as that is a different datatype
    island_replicate <- island_replicates[[island_replicate_index]]
    for (clade_index in seq_along(island_replicate)) {
      island <- island_replicate[[clade_index]]
      tryCatch(
        DAISIEmainland::check_island(island),
        error = function(e) {
          stop(
            "Error in 'island_replicates[[",
              island_replicate_index, "]][[", clade_index,"]]' \n",
            "with error message: ", e$message
          )
        }
      )
    }
  }

  # As function is mainly called for side effect, see
  # See https://style.tidyverse.org/functions.html#return
  invisible(island_replicates)
}
