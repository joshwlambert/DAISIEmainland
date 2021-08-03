#' Simulates mainland extinction as a pure-death process with replacement
#' via the speciation of an exisiting mainland species.
#'
#' @inheritParams default_params_doc
#'
#' @return a list where each element is a mainland clade, this could be a
#' single or multiple clades. Each element of the list is a matrix with nine
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
  spec_id_sample <- 1:m
  if (mainland_ex == 0) {
    timeval <- total_time
  } else {
    timeval <- stats::rexp(n = 1, rate = m * mainland_ex)
  }
  while (timeval < total_time) {
    #EXTINCTION
    extinct_spec <- DDD::sample2(spec_id_sample, 1)
    clade <- c()
    for (i in seq_along(mainland)) {
      clade[i] <- any(mainland[[i]][, "spec_id"] == extinct_spec)
    }
    clade <- which(clade)
    spec_to_die <- which(mainland[[clade]][, "spec_id"] == extinct_spec)

    mainland[[clade]][spec_to_die, "spec_type"] <- "E"
    mainland[[clade]][spec_to_die, "spec_ex_t"] <- timeval

    index_to_remove <- which(spec_id_sample == extinct_spec)
    testit::assert(length(index_to_remove) == 1)
    spec_id_sample <- spec_id_sample[-index_to_remove]
    testit::assert(length(spec_id_sample) == m - 1)

    # REPLACEMENT
    branch_spec <- DDD::sample2(spec_id_sample, 1)
    clade <- c()
    for (i in seq_along(mainland)) {
      clade[i] <- any(mainland[[i]][, "spec_id"] == branch_spec)
    }
    clade <- which(clade)
    spec_to_split <- which(mainland[[clade]][, "spec_id"] == branch_spec)
    #CLADOGENESIS - this splits species into two new species
    #for daughter A
    oldstatus <- mainland[[clade]][spec_to_split, "branch_code"]
    mainland[[clade]][spec_to_split, "spec_type"] <- "E"
    mainland[[clade]][spec_to_split, "spec_ex_t"] <- timeval
    mainland[[clade]] <- rbind(
      mainland[[clade]],
      data.frame(spec_id = max_spec_id + 1,
                 main_anc_id = mainland[[clade]][spec_to_split, "main_anc_id"],
                 spec_type = "C",
                 branch_code = paste0(oldstatus, "A"),
                 branch_t = timeval,
                 spec_origin_t = timeval,
                 spec_ex_t = NaN))
    #for daughter B
    mainland[[clade]] <- rbind(
      mainland[[clade]],
      data.frame(spec_id = max_spec_id + 2,
                 main_anc_id = mainland[[clade]][spec_to_split, "main_anc_id"],
                 spec_type = "C",
                 branch_code = paste0(oldstatus, "B"),
                 branch_t = timeval,
                 spec_origin_t = timeval,
                 spec_ex_t = NaN))

    index_to_remove <- which(spec_id_sample == branch_spec)
    testit::assert(length(index_to_remove) == 1)
    spec_id_sample <- spec_id_sample[-index_to_remove]
    testit::assert(length(spec_id_sample) == m - 2)
    spec_id_sample <- c(spec_id_sample, max_spec_id + 1, max_spec_id + 2)
    testit::assert(length(spec_id_sample) == m)

    max_spec_id <- max_spec_id + 2

    timeval <- timeval + stats::rexp(n = 1, rate = m * mainland_ex)
  }
  mainland <- lapply(mainland, function(x) {
    x[, "spec_ex_t"][is.na(x[, "spec_ex_t"])] <- total_time
    return(x)
  })
  return(mainland)
}
