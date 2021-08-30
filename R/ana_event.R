#' Updates the island_spec and max_spec_id when an anagenesis event occurs
#'
#' @inheritParams default_params_doc
#'
#' @return A two element list
#' @author Joshua W. Lambert
ana_event <- function(island_spec,
                      max_spec_id) {

  testit::assert(is.data.frame(island_spec))
  testit::assert(ncol(island_spec) == 7)
  testit::assert(is.numeric(max_spec_id))

  immig_specs <- which(island_spec[, "spec_type"] == "I")
  if (length(immig_specs) == 1) {
    anagenesis <- immig_specs
  }
  if (length(immig_specs) > 1) {
    anagenesis <- DDD::sample2(immig_specs, 1)
  }
  max_spec_id <- max_spec_id + 1
  island_spec[anagenesis, "spec_type"] <- "A"
  island_spec[anagenesis, "spec_id"] <- max_spec_id
  island_spec[anagenesis, "ana_origin"] <- "immig_parent"
  return(list(island_spec = island_spec,
              max_spec_id = max_spec_id))
}
