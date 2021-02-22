#' Samples the mainland clade with the mainland sampling probability
#'
#' @inheritParams default_params_doc
#'
#' @return Matrix
sample_mainland <- function(
  total_time,
  mainland,
  mainland_sample_prob,
  island_spec) {
  if (mainland_sample_prob == 1) {
    return(mainland)
  } else {
    extant_spec <- which(mainland[, 4] != "E")
    sampled_spec <- stats::rbinom(n = length(extant_spec),
                                  size = 1,
                                  prob = mainland_sample_prob)
    extant_not_sampled <- extant_spec[which(sampled_spec == 0)]
    mainland[extant_not_sampled, 4] <- "NS"
    mainland[extant_not_sampled, 9] <- total_time - 1e-5
  }
  return(mainland)
}
