#' Updates state of island given sampled event for a constant rate case.
#'
#' Makes the event happen by updating island species matrix and species IDs.
#' What event happens is determined by the sampling in the algorithm.
#'
#' @inheritParams default_params_doc
#'
#' @return The updated state of the system, which is a list with the
#' \code{island_spec} matrix and an integer \code{max_spec_id} with the most recent
#' ID of species.
#'
#' @keywords internal
update_state <- function(timeval,
                         total_time,
                         possible_event,
                         max_spec_id,
                         mainland_spec,
                         island_spec) {
  # RJCB: Simplify: cyclomatic complexity is above 15
  #
  # Thanks to
  # cyclocomp::cyclocomp_package_dir()

  #IMMIGRATION
  if (possible_event == 1) {
    colonist <- DDD::sample2(mainland_spec, 1)
    if (length(island_spec[, "spec_id"]) != 0) {
      isitthere <- which(island_spec[, "spec_id"] == colonist)
    } else {
      isitthere <- c()
    }
    if (length(isitthere) == 0) {
      island_spec <- rbind(
        island_spec,
        data.frame(spec_id = colonist,
                   main_anc_id = colonist,
                   col_t = timeval,
                   spec_type = "I",
                   branch_code = NA,
                   branch_t = NA,
                   ana_origin = NA))
    }
    if (length(isitthere) != 0) {
      island_spec[isitthere, ] <- data.frame(spec_id = colonist,
                                             main_anc_id = colonist,
                                             col_t = timeval,
                                             spec_type = "I",
                                             branch_code = NA,
                                             branch_t = NA,
                                             ana_origin = NA)
    }
  }

  #EXTINCTION
  if (possible_event == 2) {
    # RJCB: instead of
    #
    #   1:length(island_spec[, "spec_id"])
    #
    # use
    #
    #   seq_len(length(island_spec[, "spec_id"]))
    #
    # as this is less error prone. Just do 'seq_len(0)' and '1:0' to
    # see for yourself why.
    #
    # Thanks to lintr::lint_package()
    extinct <- DDD::sample2(1:length(island_spec[, "spec_id"]), 1)
    #this chooses the row of species data to remove
    typeofspecies <- island_spec[extinct, "spec_type"]
    if (typeofspecies == "I") {
      island_spec <- island_spec[-extinct, ]
    }
    #remove immigrant
    if (typeofspecies == "A") {
      island_spec <- island_spec[-extinct, ]
    }
    #remove anagenetic
    if (typeofspecies == "C") {
      #remove cladogenetic
      #first find species with same ancestor AND arrival total_time
      sisters <- intersect(which(island_spec[, "main_anc_id"] ==
                                   island_spec[extinct, "main_anc_id"]),
                           which(island_spec[, "col_t"] ==
                                   island_spec[extinct, "col_t"]))
      survivors <- sisters[which(sisters != extinct)]
      if (length(sisters) == 2) {
        #survivors status becomes anagenetic
        island_spec[survivors, "spec_type"] <- "A"
        island_spec[survivors, "branch_code"] <- NA
        island_spec[survivors, "branch_t"] <- NA
        island_spec[survivors, "ana_origin"] <- "Clado_extinct"
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
          survivors[which(substring(island_spec[survivors, "branch_code"], 1, numberofsplits) == motiftofind)]
        #different rules depending on whether a B or A is removed. B going extinct is simpler because it only
        #carries a record of the most recent speciation
        if (mostrecentspl == "A") {
          #change the splitting date of the sister species so that it inherits the early splitting that used to belong to A.
          # Bug fix here thanks to Nadiah Kristensen: max -> min
          tochange <-
            possiblesister[which(island_spec[possiblesister, "branch_t"] == min(as.numeric(island_spec[possiblesister, "branch_t"])))]
          island_spec[tochange, "branch_t"] <- island_spec[extinct, "branch_t"]
        }
        #remove the offending A/B from these species
        island_spec[possiblesister, "branch_code"] <-
          paste0(substring(island_spec[possiblesister, "branch_code"], 1, numberofsplits - 1),
                substring(island_spec[possiblesister, "branch_code"], numberofsplits + 1,
                          nchar(island_spec[possiblesister, "branch_code"])))
        island_spec <- island_spec[-extinct, ]
      }
    }
  }

  #ANAGENESIS
  if (possible_event == 3) {
    immi_specs <- which(island_spec[, "spec_type"] == "I")
    #we only allow immigrants to undergo anagenesis
    if(length(immi_specs) == 1) {
      anagenesis <- immi_specs
    }
    if (length(immi_specs) > 1) {
      anagenesis <- DDD::sample2(immi_specs, 1)
    }
    max_spec_id <- max_spec_id + 1
    island_spec[anagenesis, "spec_type"] <- "A"
    island_spec[anagenesis, "spec_id"] <- max_spec_id
    island_spec[anagenesis, "ana_origin"] <- "Immig_parent"
  }

  #CLADOGENESIS - this splits species into two new species - both of which receive
  if (possible_event == 4) {
    # RJCB: instead of
    #
    #   1:length(island_spec[, "spec_id"])
    #
    # use
    #
    #   seq_len(length(island_spec[, "spec_id"]))
    #
    # as this is less error prone. Just do 'seq_len(0)' and '1:0' to
    # see for yourself why.
    #
    # Thanks to lintr::lint_package()
    tosplit <- DDD::sample2(1:length(island_spec[, "spec_id"]), 1)
    #if the species that speciates is cladogenetic
    if (island_spec[tosplit, "spec_type"] == "C") {
      #for daughter A
      island_spec[tosplit, "spec_type"] <- "C"
      island_spec[tosplit, "spec_id"] <- max_spec_id + 1
      oldstatus <- island_spec[tosplit, "branch_code"]
      island_spec[tosplit, "branch_code"] <- paste0(oldstatus, "A")
      #island_spec[tosplit,6] = timeval
      island_spec[tosplit, "ana_origin"] <- NA
      #for daughter B
      island_spec <- rbind(
        island_spec,
        data.frame(spec_id = max_spec_id + 2,
                   main_anc_id = island_spec[tosplit, "main_anc_id"],
                   col_t = island_spec[tosplit, "col_t"],
                   spec_type = "C",
                   branch_code =  paste0(oldstatus, "B"),
                   branch_t = timeval,
                   ana_origin = NA))
      max_spec_id <- max_spec_id + 2
    } else {
      #if the species that speciates is not cladogenetic
      #for daughter A
      island_spec[tosplit, "spec_type"] <- "C"
      island_spec[tosplit, "spec_id"] <- max_spec_id + 1
      island_spec[tosplit, "branch_code"] <- "A"
      island_spec[tosplit, "branch_t"] <- island_spec[tosplit, 3]
      island_spec[tosplit, "ana_origin"] <- NA
      #for daughter B
      island_spec <- rbind(
        island_spec,
        data.frame(spec_id = max_spec_id + 2,
                   main_anc_id = island_spec[tosplit, "main_anc_id"],
                   col_t = island_spec[tosplit, "col_t"],
                   spec_type = "C",
                   branch_code =  "B",
                   branch_t = timeval,
                   ana_origin = NA))
      max_spec_id <- max_spec_id + 2
    }
  }

  updated_state <- list(island_spec = island_spec,
                        max_spec_id = max_spec_id)
  return(updated_state)
}
