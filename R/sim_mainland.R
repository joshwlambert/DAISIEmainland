#' Simulates mainland extinction as a pure-death process with replacement
#' via the speciation of an exisiting mainland species.
#'
#' @inheritParams default_params_doc
#'
#' @return a list where each element is a mainland lineage, this could be a
#' single or multiple lineages. Each element of the list is a matrix with nine
#' columns. The columns contain
#' \enumerate{
#'     \item Species identity
#'     \item Mainland ancestor identity
#'     \item Colonisation time
#'     \item Species type
#'     \item Branching code
#'     \item Branching time (forwards in time from the start of the simulation)
#'     \item Anagenetic origin
#'     \item Species origination time
#'     \item Species extinction time
#' }
#' @keywords internal
sim_mainland <- function(
  time,
  m,
  mainland_ext
) {
  total_time <- time
  time <- 0
  max_spec_id <- m
  mainland <- vector(mode = "list", length = m)
  for (i in 1:m) {
    mainland[[i]] <- data.frame(spec_id = i,
                                main_anc_id = i,
                                col_t = 0,
                                spec_type = "I",
                                branch_code = "A",
                                branch_t = NA,
                                ana_origin = NA,
                                spec_origin_t = 0,
                                spec_ex_t = 0)
  }
  if (mainland_ext == 0) {
    time <- total_time
  } else {
    time <- stats::rexp(n = 1, rate = m * mainland_ext)
  }
  while (time < total_time) {
    #EXTINCTION
    spec_id <- c()
    spec_type <- c()
    for (i in seq_along(mainland)) {
      spec_id <- c(spec_id, mainland[[i]][, "spec_id"])
      spec_type <- c(spec_type, mainland[[i]][, "spec_type"])
    }
    if (any(spec_type == "E")) {
      spec_id <- spec_id[-which(spec_type == "E")]
    }
    extinct_spec <- DDD::sample2(spec_id, 1)
    lineage <- c()
    for (i in seq_along(mainland)) {
      lineage[i] <- any(mainland[[i]][, "spec_id"] == extinct_spec)
    }
    lineage <- which(lineage)
    extinct <- which(mainland[[lineage]][, "spec_id"] == extinct_spec)
    ex_spec_type <- mainland[[lineage]][extinct, "spec_type"]
    if (ex_spec_type == "I" || ex_spec_type == "A") {
      mainland[[lineage]][extinct, "spec_type"] <- "E"
      mainland[[lineage]][extinct, "spec_ex_t"] <- time
    }
    # FIND IF ANA_ORIGIN IS EVER CHANGED
    if (ex_spec_type == "C") {
      #first find species with same ancestor AND arrival total_time
      sisters <- intersect(which(mainland[[lineage]][, "main_anc_id"] ==
                                   mainland[[lineage]][extinct, "main_anc_id"]),
                           which(mainland[[lineage]][, "col_t"] ==
                                   mainland[[lineage]][extinct, "col_t"]))
      survivors <- sisters[which(sisters != extinct)]
      if (length(sisters) == 2) {
        #survivors status becomes anagenetic
        mainland[[lineage]][survivors, "spec_type"] <- "A"
        mainland[[lineage]][survivors, "ana_origin"] <- "Clado_extinct"
        mainland[[lineage]][extinct, "spec_type"] <- "E"
        mainland[[lineage]][extinct, "spec_ex_t"] <- time
      }

      if (length(sisters) >= 3) {
        numberofsplits <- nchar(mainland[[lineage]][extinct, "branch_code"])
        mostrecentspl <- substring(mainland[[lineage]][extinct, "branch_code"],
                                   numberofsplits)

        if (mostrecentspl == "B") {
          sistermostrecentspl <- "A"
        }
        if (mostrecentspl == "A") {
          sistermostrecentspl <- "B"
        }
        motiftofind <- paste(substring(
          mainland[[lineage]][extinct, "branch_code"],
          1,
          numberofsplits - 1),
          sistermostrecentspl,
          sep = "")
        possiblesister <- survivors[which(substring(
          mainland[[lineage]][survivors, "branch_code"],
          1,
          numberofsplits) == motiftofind)]
        #different rules depending on whether a B or A is removed.
        #B going extinct is simpler because it only
        #carries a record of the most recent speciation
        if (mostrecentspl == "A") {
          #change the splitting date of the sister species so that it inherits
          #the early splitting that used to belong to A.
          # Bug fix here thanks to Nadiah Kristensen: max -> min
          tochange <-
            possiblesister[which(
              mainland[[lineage]][possiblesister, "branch_t"] ==
                min(as.numeric(mainland[[lineage]][possiblesister, "branch_t"])))]
          mainland[[lineage]][tochange, "branch_t"] <-
            mainland[[lineage]][extinct, "branch_t"]
        }
        #change the offending A/B from these species to E
        mainland[[lineage]][extinct, "spec_type"] <- "E"
        mainland[[lineage]][extinct, "spec_ex_t"] <- time
      }
    }
    # REPLACEMENT
    spec_id <- c()
    spec_type <- c()
    for (i in seq_along(mainland)) {
      spec_id <- c(spec_id, mainland[[i]][, "spec_id"])
      spec_type <- c(spec_type, mainland[[i]][, "spec_type"])
    }
    if (any(spec_type == "E")) {
      spec_id <- spec_id[-which(spec_type == "E")]
    }
    branch_spec <- DDD::sample2(spec_id, 1)
    lineage <- c()
    for (i in seq_along(mainland)) {
      lineage[i] <- any(mainland[[i]][, "spec_id"] == branch_spec)
    }
    lineage <- which(lineage)
    tosplit <- which(mainland[[lineage]][, "spec_id"] == branch_spec)
    #CLADOGENESIS - this splits species into two new species
    #for daughter A
    oldstatus <- mainland[[lineage]][tosplit, "branch_code"]
    mainland[[lineage]][tosplit, "spec_type"] <- "E"
    mainland[[lineage]][tosplit, "spec_ex_t"] <- time
    mainland[[lineage]] <- rbind(
      mainland[[lineage]],
      data.frame(spec_id = max_spec_id + 1,
                 main_anc_id = mainland[[lineage]][tosplit, "main_anc_id"],
                 col_t = mainland[[lineage]][tosplit, "col_t"],
                 spec_type = "C",
                 branch_code = paste0(oldstatus, "A"),
                 branch_t = time,
                 ana_origin = NA,
                 spec_origin_t = time,
                 spec_ex_t = NA))
    #for daughter B
    mainland[[lineage]] <- rbind(
      mainland[[lineage]],
      data.frame(spec_id = max_spec_id + 2,
                 main_anc_id = mainland[[lineage]][tosplit, "main_anc_id"],
                 col_t = mainland[[lineage]][tosplit, "col_t"],
                 spec_type = "C",
                 branch_code = paste0(oldstatus, "B"),
                 branch_t = time,
                 ana_origin = NA,
                 spec_origin_t = time,
                 spec_ex_t = NA))
    max_spec_id <- max_spec_id + 2
    time <- time + stats::rexp(n = 1, rate = m * mainland_ext)
  }
  for (i in seq_along(mainland)) {
    for (j in seq_len(nrow(mainland[[i]]))) {
      if (is.na(mainland[[i]][j, 9])) {
        mainland[[i]][j, 9] <- total_time
      }
    }
  }
  return(mainland)
}
