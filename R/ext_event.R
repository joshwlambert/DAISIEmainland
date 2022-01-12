#' Updates the island_tbl when an extinction event occurs
#'
#' @inheritParams default_params_doc
#'
#' @return A data frame
#' @author Joshua W. Lambert
ext_event <- function(island_tbl) {
  testit::assert(is.data.frame(island_tbl))
  testit::assert(ncol(island_tbl) == 7)
  testit::assert(nrow(island_tbl) > 0)

  extinct <- DDD::sample2(seq_len(length(island_tbl[, "spec_id"])), 1)
  typeofspecies <- island_tbl[extinct, "spec_type"]
  if (typeofspecies == "I" || typeofspecies == "A") {
    island_tbl <- island_tbl[-extinct, ]
  }
  if (typeofspecies == "C") {
    # first find species with same ancestor AND arrival total_time
    sisters <- intersect(
      which(island_tbl[, "main_anc_id"] ==
        island_tbl[extinct, "main_anc_id"]),
      which(island_tbl[, "col_t"] ==
        island_tbl[extinct, "col_t"])
    )
    survivors <- sisters[which(sisters != extinct)]
    if (length(sisters) == 2) {
      # survivors status becomes anagenetic
      island_tbl[survivors, "spec_type"] <- "A"
      island_tbl[survivors, "branch_code"] <- as.character(NA)
      island_tbl[survivors, "branch_t"] <- NaN
      island_tbl[survivors, "ana_origin"] <- "clado_extinct"
      island_tbl <- island_tbl[-extinct, ]
    }

    if (length(sisters) >= 3) {
      numberofsplits <- nchar(island_tbl[extinct, "branch_code"])
      mostrecentspl <- substring(
        island_tbl[extinct, "branch_code"],
        numberofsplits
      )

      if (mostrecentspl == "B") {
        sistermostrecentspl <- "A"
      }
      if (mostrecentspl == "A") {
        sistermostrecentspl <- "B"
      }
      motiftofind <-
        paste0(
          substring(
            island_tbl[extinct, "branch_code"],
            1,
            numberofsplits - 1
          ),
          sistermostrecentspl
        )
      possiblesister <-
        survivors[which(substring(
          island_tbl[survivors, "branch_code"],
          1,
          numberofsplits
        ) == motiftofind)]
      # different rules depending on whether a B or A is removed. B going
      # extinct is simpler because it only carries a record of the most
      # recent speciation
      if (mostrecentspl == "A") {
        # change the splitting date of the sister species so that it inherits
        # the early splitting that used to belong to A.
        # Bug fix here thanks to Nadiah Kristensen: max -> min
        tochange <- possiblesister[which(
          island_tbl[possiblesister, "branch_t"] ==
            min(island_tbl[possiblesister, "branch_t"])
        )]
        island_tbl[tochange, "branch_t"] <- island_tbl[extinct, "branch_t"]
      }
      # remove the offending A/B from these species
      island_tbl[possiblesister, "branch_code"] <-
        paste0(
          substring(
            island_tbl[possiblesister, "branch_code"],
            1,
            numberofsplits - 1
          ),
          substring(
            island_tbl[possiblesister, "branch_code"],
            numberofsplits + 1,
            nchar(island_tbl[possiblesister, "branch_code"])
          )
        )
      island_tbl <- island_tbl[-extinct, ]
    }
  }
  return(island_tbl)
}
