#' Updates the island_spec when an extinction event occurs
#'
#' @inheritParams default_params_doc
#'
#' @return A data frame
#' @author Joshua W. Lambert
ext_event <- function(island_spec) {

  testit::assert(is.data.frame(island_spec))
  testit::assert(ncol(island_spec) == 7)
  testit::assert(nrow(island_spec) > 0)

  extinct <- DDD::sample2(seq_len(length(island_spec[, "spec_id"])), 1)
  typeofspecies <- island_spec[extinct, "spec_type"]
  if (typeofspecies == "I" || typeofspecies == "A") {
    island_spec <- island_spec[-extinct, ]
  }
  if (typeofspecies == "C") {
    #first find species with same ancestor AND arrival total_time
    sisters <- intersect(which(island_spec[, "main_anc_id"] ==
                                 island_spec[extinct, "main_anc_id"]),
                         which(island_spec[, "col_t"] ==
                                 island_spec[extinct, "col_t"]))
    survivors <- sisters[which(sisters != extinct)]
    if (length(sisters) == 2) {
      #survivors status becomes anagenetic
      island_spec[survivors, "spec_type"] <- "A"
      island_spec[survivors, "branch_code"] <- as.character(NA)
      island_spec[survivors, "branch_t"] <- NaN
      island_spec[survivors, "ana_origin"] <- "clado_extinct"
      island_spec <- island_spec[-extinct, ]
    }

    if (length(sisters) >= 3) {
      numberofsplits <- nchar(island_spec[extinct, "branch_code"])
      mostrecentspl <- substring(island_spec[extinct, "branch_code"],
                                 numberofsplits)

      if (mostrecentspl == "B") {
        sistermostrecentspl <- "A"
      }
      if (mostrecentspl == "A") {
        sistermostrecentspl <- "B"
      }
      motiftofind <-
        paste0(substring(island_spec[extinct, "branch_code"],
                         1,
                         numberofsplits - 1),
               sistermostrecentspl)
      possiblesister <-
        survivors[which(substring(island_spec[survivors, "branch_code"],
                                  1,
                                  numberofsplits) == motiftofind)]
      #different rules depending on whether a B or A is removed. B going
      #extinct is simpler because it only carries a record of the most
      #recent speciation
      if (mostrecentspl == "A") {
        #change the splitting date of the sister species so that it inherits
        #the early splitting that used to belong to A.
        #Bug fix here thanks to Nadiah Kristensen: max -> min
        tochange <- possiblesister[which(
          island_spec[possiblesister, "branch_t"] ==
            min(island_spec[possiblesister, "branch_t"]))]
        island_spec[tochange, "branch_t"] <- island_spec[extinct, "branch_t"]
      }
      #remove the offending A/B from these species
      island_spec[possiblesister, "branch_code"] <-
        paste0(substring(island_spec[possiblesister, "branch_code"],
                         1,
                         numberofsplits - 1),
               substring(island_spec[possiblesister, "branch_code"],
                         numberofsplits + 1,
                         nchar(island_spec[possiblesister, "branch_code"])))
      island_spec <- island_spec[-extinct, ]
    }
  }
  return(island_spec)
}
