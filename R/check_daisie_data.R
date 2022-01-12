#' Check if a `daisie_data` (i.e. a `DAISIE` `datalist`) is valid.
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
#' daisie_mainland_data <- sim_island_with_mainland(
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
#' ideal_multi_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data
#'
#' daisie_data <- ideal_multi_daisie_data[[1]]
#' check_daisie_data(daisie_data)
#' @author Richèl J.C. Bilderbeek
#'
#' @export
check_daisie_data <- function(daisie_data) {
  testit::assert(is.list(daisie_data))
  testit::assert(length(daisie_data) > 0)

  # The first element, is the meta data
  daisie_data_meta_data <- daisie_data[[1]]
  testit::assert("island_age" %in% names(daisie_data_meta_data))
  testit::assert(
    "not_present" %in% names(daisie_data_meta_data) ||
      all(c("not_present_type1", "not_present_type2") %in%
        names(daisie_data_meta_data))
  )

  if (length(daisie_data) == 1) {
    return(invisible(daisie_data))
  }

  # '2' as the first element is special
  for (colonist_index in seq(2, length(daisie_data))) {
    # colonist_index '2' is the index of the first colonist
    colonist_lineage <- daisie_data[[colonist_index]]
    testit::assert("branching_times" %in% names(colonist_lineage))
    testit::assert("stac" %in% names(colonist_lineage))
    testit::assert("missing_species" %in% names(colonist_lineage))
    # optional names are 'colonist_name' and 'type1or2'
  }

  invisible(daisie_data)
}
