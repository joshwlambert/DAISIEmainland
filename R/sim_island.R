#' Simulates the island given rates and a mainland clade.
#'
#' @inheritParams default_params_doc
#'
#' @return A list of two elements. The first element is \code{ideal_island},
#' and the second element is \code{empirical_island}. Within each of these
#' is a list containing 3 components:
#' \itemize{
#'   \item{\code{$branching_times}: island age and stem age of the
#'     population/species in the case of Non-endemic, Non-endemic_MaxAge and
#'     Endemic anagenetic species.
#'
#'     For cladogenetic species these should
#'     be island age and branching times of the radiation including the
#'     stem age of the radiation.}
#'   \item{\code{$stac}: An integer ranging from 1 to 6
#'   indicating the status of the colonist:}
#'   \enumerate{
#'     \item Non_endemic_MaxAge
#'     \item Endemic
#'     \item Endemic&Non_Endemic
#'     \item Non_endemic_MaxAge
#'     \item Endemic_singleton_MaxAge
#'     \item Endemic_clade_MaxAge
#' }
#' \item{\code{$missing_species}: number of island species that were
#' not sampled for particular clade (only applicable for endemic clades)}
#' }
#'
#' @keywords internal
sim_island <- function(
  total_time,
  island_pars,
  mainland_clade,
  mainland_sample_prob
) {
browser()
  testit::assert(is.numeric(total_time))
  testit::assert(total_time > 0)
  testit::assert(is.numeric(island_pars))
  testit::assert(length(island_pars) == 5)
  testit::assert(island_pars[4] > 0)
  testit::assert(is.data.frame(mainland_clade))
  testit::assert(is.numeric(mainland_sample_prob))
  testit::assert(mainland_sample_prob >= 0 && mainland_sample_prob <= 1)

  # Initialization
  timeval <- 0

  mainland_spec <- mainland_clade[which(
    mainland_clade[, "spec_origin_t"] <= timeval &
      mainland_clade[, "spec_ex_t"] > timeval), "spec_id"]
  mainland_n <- length(mainland_spec)
  max_spec_id <- max(mainland_clade[, "spec_id"])

  mainland_exts <- mainland_clade[, "spec_ex_t"]
  mainland_brts <- mainland_clade[, "spec_origin_t"]
  mainland_event_t <- c(mainland_exts, mainland_brts, total_time + 1)
  mainland_event_t <- unique(mainland_event_t)
  mainland_event_t <- sort(mainland_event_t, decreasing = FALSE)
  mainland_event_t <- mainland_event_t[mainland_event_t != 0]
  mainland_event_t <- mainland_event_t[mainland_event_t != total_time]

  island_spec <- data.frame(
    spec_id = numeric(),
    main_anc_id = numeric(),
    col_t = numeric(),
    spec_type = character(),
    branch_code = character(),
    branch_t = numeric(),
    ana_origin = character()
  )

  lac <- island_pars[1]
  mu <- island_pars[2]
  k <- island_pars[3]
  gam <- island_pars[4]
  laa <- island_pars[5]

  num_spec <- 0
  num_immigrants <- 0

  # Start Monte Carlo iterations
  while (timeval < total_time) {
    rates <- calc_rates(
      gam = gam,
      laa = laa,
      lac = lac,
      mu = mu,
      k = k,
      num_spec = num_spec,
      num_immigrants = num_immigrants,
      mainland_n = mainland_n)

    totalrate <- rates$immig_rate + rates$ext_rate +
      rates$ana_rate + rates$clado_rate
    if (totalrate != 0) {
      dt <- stats::rexp(1, totalrate)
      timeval <- timeval + dt
    } else {
      timeval <- total_time + 1
    }

    # If a mainland speciation event has occurred since the last time step
    if (timeval > mainland_event_t[1]) {
      timeval <- mainland_event_t[1]
      mainland_event_t <- mainland_event_t[-1]
      mainland_spec <- mainland_clade[which(
        (mainland_clade[, "spec_origin_t"]) <= timeval &
          mainland_clade[, "spec_ex_t"] > timeval), "spec_id"]
      mainland_n <- length(mainland_spec)

    } else {

      # Changes island species to endemic when a mainland species goes extinct
      island_spec <- update_island_endemics(
        timeval = timeval,
        total_time = total_time,
        island_spec = island_spec,
        mainland_clade = mainland_clade)
      num_immigrants <- length(which(island_spec[, "spec_type"] == "I"))

      if (timeval <= total_time) {
        rates <- calc_rates(
          gam = gam,
          laa = laa,
          lac = lac,
          mu = mu,
          k = k,
          num_spec = num_spec,
          num_immigrants = num_immigrants,
          mainland_n = mainland_n)

        totalrate <- rates$immig_rate + rates$ext_rate +
          rates$ana_rate + rates$clado_rate
        if (totalrate == 0) {
          timeval <- total_time + 1

        } else {

          possible_event <- sample_event(
            rates = rates)

          updated_state <- update_state(
            timeval = timeval,
            total_time = total_time,
            possible_event = possible_event,
            max_spec_id = max_spec_id,
            mainland_spec = mainland_spec,
            island_spec = island_spec)

          island_spec <- updated_state$island_spec
          max_spec_id <- updated_state$max_spec_id
          num_spec <- nrow(island_spec)
          num_immigrants <- length(which(island_spec[, "spec_type"] == "I"))
        }
      }
    }
  }

  island <- create_island(
    total_time = total_time,
    island_spec = island_spec,
    mainland_clade = mainland_clade,
    mainland_sample_prob = mainland_sample_prob)
  return(island)
}
