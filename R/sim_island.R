#' Simulates the island.
#'
#' @inheritParams default_params_doc
#'
#' @return List
#' @keywords internal
sim_island <- function(
  time,
  m,
  island_pars,
  mainland,
  mainland_sample_prob
) {

  #### Initialization ####
  timeval <- 0
  total_time <- time

  mainland_spec <- as.numeric(mainland[which(
    as.numeric(mainland[, 8]) <= timeval &
      as.numeric(mainland[, 9]) > timeval), 1])
  mainland_n <- length(mainland_spec)
  max_spec_id <- max(as.numeric(mainland[, 1]))

  mainland_exts <- as.numeric(mainland[, 9])
  mainland_brts <- as.numeric(mainland[, 8])
  mainland_event_t <- c(mainland_exts, mainland_brts, total_time + 1)
  mainland_event_t <- unique(mainland_event_t)
  mainland_event_t <- sort(mainland_event_t, decreasing = FALSE)
  mainland_event_t <- mainland_event_t[mainland_event_t != 0]
  mainland_event_t <- mainland_event_t[mainland_event_t != total_time]

  # CHECK MAINLAND_EVENT_T

  island_spec <- data.frame(spec_id = numeric(),
                            main_anc_id = numeric(),
                            col_t = numeric(),
                            spec_type = character(),
                            branch_code = character(),
                            branch_t = numeric(),
                            ana_origin = character())


  lac <- island_pars[1]
  mu <- island_pars[2]
  k <- island_pars[3]
  gam <- island_pars[4]
  laa <- island_pars[5]

  num_spec <- 0
  num_immigrants <- 0

  #### Start Monte Carlo iterations ####
  while (timeval < total_time) {
    rates <- calc_rates(
      timeval = timeval,
      total_time = total_time,
      gam = gam,
      laa = laa,
      lac = lac,
      mu = mu,
      k = k,
      num_spec = num_spec,
      num_immigrants = num_immigrants,
      mainland_n = mainland_n
    )

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
      mainland_spec <- mainland[which(
        (mainland[, "spec_origin_t"]) <= timeval &
          mainland[, "spec_ex_t"] > timeval), 1]
      mainland_n <- length(mainland_spec)

    } else {

      # Changes island species to endemic when a mainland species goes extinct
      island_state <- update_island_endemics(
        timeval = timeval,
        total_time = total_time,
        island_spec = island_spec,
        mainland = mainland)
      num_spec <- nrow(island_spec)
      num_immigrants <- length(which(island_spec[, "spec_type"] == "I"))

      if (timeval <= total_time) {
        rates <- calc_rates(
          timeval = timeval,
          total_time = total_time,
          gam = gam,
          laa = laa,
          lac = lac,
          mu = mu,
          k = k,
          num_spec = num_spec,
          num_immigrants = num_immigrants,
          mainland_n = mainland_n
        )

        totalrate <- rates$immig_rate + rates$ext_rate +
          rates$ana_rate + rates$clado_rate
        if (totalrate == 0) {
          timeval <- total_time + 1

        } else {

          possible_event <- sample_event(
            rates = rates
          )

          updated_state <- update_state(
            timeval = timeval,
            total_time = total_time,
            possible_event = possible_event,
            max_spec_id = max_spec_id,
            mainland_spec = mainland_spec,
            island_spec = island_spec
          )

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
    mainland = mainland,
    mainland_sample_prob = mainland_sample_prob)
  return(island)
}
