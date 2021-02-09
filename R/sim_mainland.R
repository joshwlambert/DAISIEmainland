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
  totaltime <- time
  time <- 0
  max_spec_id <- m
  mainland <- vector(mode = "list", length = m)
  for (i in 1:m) {
    mainland[[i]] <- matrix(c(i, i, 0, "I", "A", NA, NA, 0, NA), nrow = 1)
  }
  if (mainland_ext == 0) {
    time <- totaltime
  } else {
    time <- stats::rexp(n = 1, rate = m * mainland_ext)
  }
  while (time < totaltime) {
    #EXTINCTION
    spec_id_num <- c()
    spec_id_let <- c()
    for (i in seq_along(mainland)) {
      spec_id_num <- as.numeric(c(spec_id_num, mainland[[i]][, 1]))
      spec_id_let <- c(spec_id_let, mainland[[i]][, 4])
    }
    if (any(spec_id_let == "E")) {
      spec_id_num <- spec_id_num[-which(spec_id_let == "E")]
    }
    extinct_spec <- DDD::sample2(spec_id_num, 1)
    lineage <- c()
    for (i in seq_along(mainland)) {
      lineage[i] <- any(as.numeric(mainland[[i]][, 1]) == extinct_spec)
    }
    lineage <- which(lineage)
    extinct <- which(mainland[[lineage]][, 1] == extinct_spec)
    typeofspecies <- mainland[[lineage]][extinct, 4]
    if (typeofspecies == "I" || typeofspecies == "A") {
      mainland[[lineage]][extinct, 4] <- "E"
      mainland[[lineage]][extinct, 9] <- time
    }
    if (typeofspecies == "C") {
      #first find species with same ancestor AND arrival totaltime
      sisters <- intersect(which(mainland[[lineage]][, 2] ==
                                   mainland[[lineage]][extinct, 2]),
                           which(mainland[[lineage]][, 3] ==
                                   mainland[[lineage]][extinct, 3]))
      survivors <- sisters[which(sisters != extinct)]
      if (length(sisters) == 2) {
        #survivors status becomes anagenetic
        mainland[[lineage]][survivors, 4] <- "A"
        mainland[[lineage]][survivors, 7] <- "Clado_extinct"
        mainland[[lineage]][extinct, 4] <- "E"
        mainland[[lineage]][extinct, 9] <- time
      }

      if (length(sisters) >= 3) {
        numberofsplits <- nchar(mainland[[lineage]][extinct, 5])
        mostrecentspl <- substring(mainland[[lineage]][extinct, 5],
                                   numberofsplits)

        if (mostrecentspl == "B") {
          sistermostrecentspl <- "A"
        }
        if (mostrecentspl == "A") {
          sistermostrecentspl <- "B"
        }
        motiftofind <- paste(substring(
          mainland[[lineage]][extinct, 5],
          1,
          numberofsplits - 1),
          sistermostrecentspl,
          sep = "")
        possiblesister <- survivors[which(substring(
          mainland[[lineage]][survivors, 5],
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
              mainland[[lineage]][possiblesister, 6] ==
                min(as.numeric(mainland[[lineage]][possiblesister, 6])))]
          mainland[[lineage]][tochange, 6] <-
            mainland[[lineage]][extinct, 6]
        }
        #change the offending A/B from these species to E
        mainland[[lineage]][extinct, 4] <- "E"
        mainland[[lineage]][extinct, 9] <- time
      }
    }
    # REPLACEMENT
    spec_id_num <- c()
    spec_id_let <- c()
    for (i in seq_along(mainland)) {
      spec_id_num <- as.numeric(c(spec_id_num, mainland[[i]][, 1]))
      spec_id_let <- c(spec_id_let, mainland[[i]][, 4])
    }
    if (any(spec_id_let == "E")) {
      spec_id_num <- spec_id_num[-which(spec_id_let == "E")]
    }
    branch_spec <- DDD::sample2(spec_id_num, 1)
    lineage <- c()
    for (i in seq_along(mainland)) {
      lineage[i] <- any(as.numeric(mainland[[i]][, 1]) == branch_spec)
    }
    lineage <- which(lineage)
    tosplit <- which(mainland[[lineage]][, 1] == branch_spec)
    #CLADOGENESIS - this splits species into two new species
    #for daughter A
    oldstatus <- mainland[[lineage]][tosplit, 5]
    mainland[[lineage]][tosplit, 4] <- "E"
    mainland[[lineage]][tosplit, 9] <- time
    mainland[[lineage]] <- rbind(
      mainland[[lineage]],
      c(max_spec_id + 1,
        mainland[[lineage]][tosplit, 2],
        mainland[[lineage]][tosplit, 3],
        "C",
        paste(oldstatus, "A", sep = ""),
        time,
        NA,
        NA,
        NA))
    #for daughter B
    mainland[[lineage]] <- rbind(
      mainland[[lineage]],
      c(max_spec_id + 2,
        mainland[[lineage]][tosplit, 2],
        mainland[[lineage]][tosplit, 3],
        "C",
        paste(oldstatus, "B", sep = ""),
        time,
        NA,
        NA,
        NA))
    max_spec_id <- max_spec_id + 2
    time <- time + stats::rexp(n = 1, rate = m * mainland_ext)
  }
  for (i in seq_along(mainland)) {
    for (j in seq_len(nrow(mainland[[i]]))) {
      if (is.na(mainland[[i]][j, 9])) {
        mainland[[i]][j, 9] <- totaltime
      }
      if (is.na(mainland[[i]][j, 6])) {
        mainland[[i]][j, 8] <- mainland[[i]][j, 3]
      } else {
        mainland[[i]][j, 8] <- mainland[[i]][j, 6]
      }
    }
  }
  return(mainland)
}
