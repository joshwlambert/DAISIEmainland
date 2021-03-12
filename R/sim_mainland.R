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
  total_time,
  m,
  mainland_ex
) {
  timeval <- 0
  max_spec_id <- m
  mainland <- vector(mode = "list", length = m)
  for (i in seq_len(m)) {
    mainland[[i]] <- data.frame(spec_id = i,
                                main_anc_id = i,
                                spec_type = "I",
                                branch_code = "A",
                                branch_t = NaN,
                                spec_origin_t = 0,
                                spec_ex_t = NA)
  }
  if (mainland_ex == 0) {
    timeval <- total_time
  } else {
    timeval <- stats::rexp(n = 1, rate = m * mainland_ex)
  }
  while (timeval < total_time) {
    #EXTINCTION
    spec_id <- unlist(lapply(mainland, function(x) x[, "spec_id"]))
    spec_type <- unlist(lapply(mainland, function(x) x[, "spec_type"]))

    spec_id <- spec_id[which(spec_type != "E")]
    extinct_spec <- DDD::sample2(spec_id, 1)
    lineage <- c()
    for (i in seq_along(mainland)) {
      lineage[i] <- any(mainland[[i]][, "spec_id"] == extinct_spec)
    }
    lineage <- which(lineage)
    extinct <- which(mainland[[lineage]][, "spec_id"] == extinct_spec)

    mainland[[lineage]][extinct, "spec_type"] <- "E"
    mainland[[lineage]][extinct, "spec_ex_t"] <- timeval

    # REPLACEMENT
    spec_id <- unlist(lapply(mainland, function(x) x[, "spec_id"]))
    spec_type <- unlist(lapply(mainland, function(x) x[, "spec_type"]))

    spec_id <- spec_id[which(spec_type != "E")]
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
    mainland[[lineage]][tosplit, "spec_ex_t"] <- timeval
    mainland[[lineage]] <- rbind(
      mainland[[lineage]],
      data.frame(spec_id = max_spec_id + 1,
                 main_anc_id = mainland[[lineage]][tosplit, "main_anc_id"],
                 spec_type = "C",
                 branch_code = paste0(oldstatus, "A"),
                 branch_t = timeval,
                 spec_origin_t = timeval,
                 spec_ex_t = NaN))
    #for daughter B
    mainland[[lineage]] <- rbind(
      mainland[[lineage]],
      data.frame(spec_id = max_spec_id + 2,
                 main_anc_id = mainland[[lineage]][tosplit, "main_anc_id"],
                 spec_type = "C",
                 branch_code = paste0(oldstatus, "B"),
                 branch_t = timeval,
                 spec_origin_t = timeval,
                 spec_ex_t = NaN))
    max_spec_id <- max_spec_id + 2
    timeval <- timeval + stats::rexp(n = 1, rate = m * mainland_ex)
  }
  mainland <- lapply(mainland, function(x) {
    x[, "spec_ex_t"][is.na(x[, "spec_ex_t"])] <- total_time
    return(x)
  })
  return(mainland)
}
