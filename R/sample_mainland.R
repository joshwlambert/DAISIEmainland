#' Samples the mainland clade with the mainland sampling probability
#'
#' @inheritParams default_params_doc
#'
#' @return A data frame
#' @author Joshua W. Lambert
sample_mainland <- function(total_time,
                            mainland_clade,
                            mainland_sample_prob,
                            mainland_sample_type,
                            island_spec) {

  if (mainland_sample_prob == 1) {
    return(mainland_clade)
  } else {
    extant_spec <- which(mainland_clade[, "spec_type"] != "E")
    sampled_spec <- stats::rbinom(n = length(extant_spec),
                                  size = 1,
                                  prob = mainland_sample_prob)
    extant_not_sampled <- extant_spec[which(sampled_spec == 0)]
    mainland_clade[extant_not_sampled, "spec_ex_t"] <- total_time - 1e-5
    if (mainland_sample_type == "undiscovered") {
      mainland_clade[extant_not_sampled, "spec_type"] <- "UD"
    } else {
      mainland_clade[extant_not_sampled, "spec_type"] <- "US"
    }
  }
  return(mainland_clade)
}
