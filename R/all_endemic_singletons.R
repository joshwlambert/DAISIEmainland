#' Checks if all island species are endemic singletons. Singletons are single
#' lineages that have not formed an island clade of two or more species.
#'
#' @inheritParams default_params_doc
#'
#' @return Boolean
#' @author Joshua W. Lambert
#' @export
#'
#' @examples
#' \dontrun{
#' island <- DAISIEmainland::sim_island_with_mainland(
#'   total_time = 1,
#'   m = 100,
#'   island_pars = c(1, 1, 10, 0.1, 1),
#'   mainland_ex = 0.1,
#'   mainland_sample_prob = 1,
#'   mainland_sample_type = "complete",
#'   replicates = 2,
#'   verbose = FALSE
#' )
#' bool <- all_endemic_singletons(island$ideal_islands[[1]])
#' }
all_endemic_singletons <- function(daisie_data) {
  testit::assert(is.list(daisie_data))
  num_island_spec <- calc_island_endemics(daisie_data = daisie_data)
  num_spec <- num_island_spec$endemics + num_island_spec$non_endemics
  if (num_spec == 0) {
    stop("island is empty")
  }
  if (num_island_spec$non_endemics >= 1) {
    return(FALSE)
  }
  daisie_data <- daisie_data[-1]
  branching_times <- lapply(daisie_data, "[[", "branching_times")
  num_spec <- unlist(lapply(branching_times, function(x) {
    length(x) - 1
  }))
  stacs <- unlist(lapply(daisie_data, "[[", "stac"))
  singletons <- all(num_spec[which(stacs != 3)] == 1)
  if (any(stacs == 3)) {
    recol <- daisie_data[which(stacs == 3)]
    all_cols <- lapply(recol, "[[", "all_colonisations")
    event_times <- lapply(all_cols, function(x) {
      lapply(x, "[[", "event_times")
    })
    num_recol_spec <- unlist(lapply(event_times, function(x) {
      lapply(x, function(y) length(y) - 1)
    }))
    recol_singletons <- all(num_recol_spec == 1)
    singletons <- singletons && recol_singletons
  }
  if (singletons) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
