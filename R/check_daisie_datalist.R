#' Check if a `daisie_datalist` (i.e. a `DAISIE` `datalist`) is valid.
#' Will \link{stop} if not.
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @note this function would better fit in the \link[DAISIE]{DAISIE} package
#'
#' @examples
#'
#' island <- sim_island_with_mainland(
#'   total_time = 1,
#'   m = 100,
#'   island_pars = c(1, 1, 50, 0.1, 1),
#'   mainland_ex = 0.5,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete",
#'   replicates = 1,
#'   verbose = FALSE
#' )
#'
#' ideal_daisie_data <- island$ideal_islands
#'
#' daisie_datalist <- ideal_daisie_data[[1]]
#' check_daisie_datalist(daisie_datalist)
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
check_daisie_datalist <- function(daisie_datalist) {
  testthat::expect_true(is.list(daisie_datalist))
  testthat::expect_true(length(daisie_datalist) > 0)

  # The first element, the 'header' is special
  daisie_datalist_header <- daisie_datalist[[1]]
  testthat::expect_true("island_age" %in% names(daisie_datalist_header))
  testthat::expect_true(
    "not_present" %in% names(daisie_datalist_header) ||
    all(c("not_present_type1", "not_present_type2") %in% names(daisie_datalist_header))
  )

  if (length(daisie_datalist) == 1) {
    return(invisible(daisie_datalist))
  }

  # '2' as the first element is special
  for (colonist_index in seq(2, length(daisie_datalist))) {
   # colonist_index '2' is the index of the first colonist
    colonist_lineage <- daisie_datalist[[colonist_index]]
    testthat::expect_true("branching_times" %in% names(colonist_lineage))
    testthat::expect_true("stac" %in% names(colonist_lineage))
    testthat::expect_true("missing_species" %in% names(colonist_lineage))
    # optional names are 'colonist_name' and 'type1or2'
  }

  # As function is mainly called for side effect, see
  # See https://style.tidyverse.org/functions.html#return
  invisible(daisie_datalist)
}
