#' Updates the island_tbl and max_spec_id when a cladogenesis event occurs
#'
#' @inheritParams default_params_doc
#'
#' @return A two element list
#' @author Joshua W. Lambert
clado_event <- function(timeval,
                        island_tbl,
                        max_spec_id) {
  testit::assert(is.numeric(timeval))
  testit::assert(is.data.frame(island_tbl))
  testit::assert(ncol(island_tbl) == 7)
  testit::assert(is.numeric(max_spec_id))

  tosplit <- DDD::sample2(seq_len(length(island_tbl[, "spec_id"])), 1)
  # if the species that speciates is cladogenetic
  if (island_tbl[tosplit, "spec_type"] == "C") {
    # for daughter A
    island_tbl[tosplit, "spec_type"] <- "C"
    island_tbl[tosplit, "spec_id"] <- max_spec_id + 1
    oldstatus <- island_tbl[tosplit, "branch_code"]
    island_tbl[tosplit, "branch_code"] <- paste0(oldstatus, "A")
    island_tbl[tosplit, "ana_origin"] <- as.character(NA)
    # for daughter B
    island_tbl <- rbind(
      island_tbl,
      data.frame(
        spec_id = max_spec_id + 2,
        main_anc_id = island_tbl[tosplit, "main_anc_id"],
        col_t = island_tbl[tosplit, "col_t"],
        spec_type = "C",
        branch_code = paste0(oldstatus, "B"),
        branch_t = timeval,
        ana_origin = as.character(NA)
      )
    )
    max_spec_id <- max_spec_id + 2
  } else {
    # if the species that speciates is not cladogenetic
    # for daughter A
    island_tbl[tosplit, "spec_type"] <- "C"
    island_tbl[tosplit, "spec_id"] <- max_spec_id + 1
    island_tbl[tosplit, "branch_code"] <- "A"
    island_tbl[tosplit, "branch_t"] <- island_tbl[tosplit, 3]
    island_tbl[tosplit, "ana_origin"] <- as.character(NA)
    # for daughter B
    island_tbl <- rbind(
      island_tbl,
      data.frame(
        spec_id = max_spec_id + 2,
        main_anc_id = island_tbl[tosplit, "main_anc_id"],
        col_t = island_tbl[tosplit, "col_t"],
        spec_type = "C",
        branch_code = "B",
        branch_t = timeval,
        ana_origin = as.character(NA)
      )
    )
    max_spec_id <- max_spec_id + 2
  }
  return(list(
    island_tbl = island_tbl,
    max_spec_id = max_spec_id
  ))
}
