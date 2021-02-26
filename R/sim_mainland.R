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
  mainland_ex
) {
  # RJCB: Simplify: cyclomatic complexity is above 15
  #
  # Thanks to
  # cyclocomp::cyclocomp_package_dir()
  total_time <- time
  time <- 0
  max_spec_id <- m
  mainland <- vector(mode = "list", length = m)
  for (i in 1:m) {
    mainland[[i]] <- data.frame(spec_id = i,
                                main_anc_id = i,
                                spec_type = "I",
                                branch_code = "A",
                                branch_t = NA,
                                spec_origin_t = 0,
                                spec_ex_t = 0)
  }
  if (mainland_ex == 0) {
    time <- total_time
  } else {
    time <- stats::rexp(n = 1, rate = m * mainland_ex)
  }
  while (time < total_time) {
    #EXTINCTION
    spec_id <- c()
    spec_type <- c()
    for (i in seq_along(mainland)) {
      # RJCB: these usages of the 'c' function result in exponentially
      # increasing runtime (well, O(0.5 n ^ 2) to be precise)
      # Prefer to use an architecture to prevent this
      spec_id <- c(spec_id, mainland[[i]][, "spec_id"])
      spec_type <- c(spec_type, mainland[[i]][, "spec_type"])
    }
    testit::assert(sum(is.na(spec_type)) == 0) # RJCB: this one fails on R 3.6.3
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

    mainland[[lineage]][extinct, "spec_type"] <- "E"
    mainland[[lineage]][extinct, "spec_ex_t"] <- time

    # REPLACEMENT
    spec_id <- c()
    spec_type <- c()
    for (i in seq_along(mainland)) {
      spec_id <- c(spec_id, mainland[[i]][, "spec_id"])
      spec_type <- c(spec_type, mainland[[i]][, "spec_type"])
    }
    testit::assert(sum(is.na(spec_type)) == 0) # RJCB: this one fails on R 3.6.3
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
                 spec_type = "C",
                 branch_code = paste0(oldstatus, "A"),
                 branch_t = time,
                 spec_origin_t = time,
                 spec_ex_t = NA))
    #for daughter B
    mainland[[lineage]] <- rbind(
      mainland[[lineage]],
      data.frame(spec_id = max_spec_id + 2,
                 main_anc_id = mainland[[lineage]][tosplit, "main_anc_id"],
                 spec_type = "C",
                 branch_code = paste0(oldstatus, "B"),
                 branch_t = time,
                 spec_origin_t = time,
                 spec_ex_t = NA))
    max_spec_id <- max_spec_id + 2
    time <- time + stats::rexp(n = 1, rate = m * mainland_ex)
  }
  for (i in seq_along(mainland)) {
    for (j in seq_len(nrow(mainland[[i]]))) {
      if (is.na(mainland[[i]][j, "spec_ex_t"])) {
        mainland[[i]][j, "spec_ex_t"] <- total_time
      }
    }
  }
  return(mainland)
}
