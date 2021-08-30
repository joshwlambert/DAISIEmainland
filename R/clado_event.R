#' Updates the island_spec and max_spec_id when a cladogenesis event occurs
#'
#' @inheritParams default_params_doc
#'
#' @return A two element list
#' @author Joshua W. Lambert
clado_event <- function(timeval,
                        island_spec,
                        max_spec_id) {

  testit::assert(is.numeric(timeval))
  testit::assert(is.data.frame(island_spec))
  testit::assert(ncol(island_spec) == 7)
  testit::assert(is.numeric(max_spec_id))

  tosplit <- DDD::sample2(seq_len(length(island_spec[, "spec_id"])), 1)
  #if the species that speciates is cladogenetic
  if (island_spec[tosplit, "spec_type"] == "C") {
    #for daughter A
    island_spec[tosplit, "spec_type"] <- "C"
    island_spec[tosplit, "spec_id"] <- max_spec_id + 1
    oldstatus <- island_spec[tosplit, "branch_code"]
    island_spec[tosplit, "branch_code"] <- paste0(oldstatus, "A")
    island_spec[tosplit, "ana_origin"] <- as.character(NA)
    #for daughter B
    island_spec <- rbind(
      island_spec,
      data.frame(spec_id = max_spec_id + 2,
                 main_anc_id = island_spec[tosplit, "main_anc_id"],
                 col_t = island_spec[tosplit, "col_t"],
                 spec_type = "C",
                 branch_code =  paste0(oldstatus, "B"),
                 branch_t = timeval,
                 ana_origin = as.character(NA)))
    max_spec_id <- max_spec_id + 2
  } else {
    #if the species that speciates is not cladogenetic
    #for daughter A
    island_spec[tosplit, "spec_type"] <- "C"
    island_spec[tosplit, "spec_id"] <- max_spec_id + 1
    island_spec[tosplit, "branch_code"] <- "A"
    island_spec[tosplit, "branch_t"] <- island_spec[tosplit, 3]
    island_spec[tosplit, "ana_origin"] <- as.character(NA)
    #for daughter B
    island_spec <- rbind(
      island_spec,
      data.frame(spec_id = max_spec_id + 2,
                 main_anc_id = island_spec[tosplit, "main_anc_id"],
                 col_t = island_spec[tosplit, "col_t"],
                 spec_type = "C",
                 branch_code =  "B",
                 branch_t = timeval,
                 ana_origin = as.character(NA)))
    max_spec_id <- max_spec_id + 2
  }
  return(list(island_spec = island_spec,
              max_spec_id = max_spec_id))
}
