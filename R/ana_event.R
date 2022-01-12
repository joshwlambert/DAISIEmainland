#' Updates the island_tbl and max_spec_id when an anagenesis event occurs
#'
#' @inheritParams default_params_doc
#'
#' @return A two element list
#' @author Joshua W. Lambert
ana_event <- function(island_tbl,
                      max_spec_id) {
  testit::assert(is.data.frame(island_tbl))
  testit::assert(ncol(island_tbl) == 7)
  testit::assert(is.numeric(max_spec_id))

  immig_specs <- which(island_tbl[, "spec_type"] == "I")
  testit::assert(length(immig_specs) >= 1)
  anagenesis <- DDD::sample2(immig_specs, 1)
  max_spec_id <- max_spec_id + 1
  island_tbl[anagenesis, "spec_type"] <- "A"
  island_tbl[anagenesis, "spec_id"] <- max_spec_id
  island_tbl[anagenesis, "ana_origin"] <- "immig_parent"
  return(list(
    island_tbl = island_tbl,
    max_spec_id = max_spec_id
  ))
}
