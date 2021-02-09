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
  totaltime <- time

  mainland_spec <- as.numeric(mainland[which(
    as.numeric(mainland[, 8]) <= timeval &
      as.numeric(mainland[, 9]) > timeval), 1])
  mainland_n <- length(mainland_spec)

  max_spec_id <- max(as.numeric(mainland[, 1]))
  if (nrow(mainland) > 1) {
    mainland_brts <- c(as.numeric(mainland[2:nrow(mainland), 8]), totaltime + 1)
    mainland_brts <- mainland_brts[-which(duplicated(mainland_brts))]
  } else {
    mainland_brts <- totaltime + 1
  }

  island_spec <- c()

  lac <- island_pars[1]
  mu <- island_pars[2]
  k <- island_pars[3]
  gam <- island_pars[4]
  laa <- island_pars[5]

  num_spec <- length(island_spec[, 1])
  num_immigrants <- length(which(island_spec[, 4] == "I"))

  #### Start Monte Carlo iterations ####
  while (timeval < totaltime) {
    rates <- calc_rates(
      timeval = timeval,
      totaltime = totaltime,
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
      timeval <- totaltime + 1
    }

    # If a mainland speciation event has occurred since the last time step
    if (timeval > mainland_brts[1]) {
      timeval <- mainland_brts[1]
      mainland_brts <- mainland_brts[-1]
    } else {
      mainland_spec <- as.numeric(mainland[which(
        as.numeric(mainland[, 8]) <= timeval &
          as.numeric(mainland[, 9]) > timeval), 1])
      mainland_n <- length(mainland_spec)

      # Changes island species to endemic when a mainland species goes extinct
      island_state <- check_island_state(timeval = timeval,
                                         totaltime = totaltime,
                                         island_spec = island_spec,
                                         mainland = mainland)
      num_spec <- length(island_spec[, 1])
      num_immigrants <- length(which(island_spec[, 4] == "I"))

      if (timeval <= totaltime) {
        rates <- calc_rates(
          timeval = timeval,
          totaltime = totaltime,
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
          timeval <- totaltime + 1

        } else {

          possible_event <- sample_event(
            rates = rates
          )

          updated_state <- update_state(
            timeval = timeval,
            totaltime = totaltime,
            possible_event = possible_event,
            max_spec_id = max_spec_id,
            mainland_spec = mainland_spec,
            island_spec = island_spec
          )

          island_spec <- updated_state$island_spec
          max_spec_id <- updated_state$max_spec_id
          num_spec <- length(island_spec[, 1])
          num_immigrants <- length(which(island_spec[, 4] == "I"))
        }
      }
    }
  }

  island <- create_island(
    totaltime = totaltime,
    island_spec = island_spec,
    mainland = mainland,
    mainland_sample_prob = mainland_sample_prob)
  return(island)
}
