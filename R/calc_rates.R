#' Calculate rates for island processes
#' @description Internal function that updates the all the rates at time t.
#'
#' @inheritParams default_params_doc
#'
#' @keywords internal
#' @return Named list with rates.
calc_rates <- function(
  timeval,
  totaltime,
  gam,
  laa,
  lac,
  mu,
  k,
  num_spec,
  num_immigrants,
  mainland_n) {
  testit::assert(is.numeric(timeval))
  testit::assert(is.numeric(totaltime))
  testit::assert(is.numeric(gam))
  testit::assert(is.numeric(laa))
  testit::assert(is.numeric(lac))
  testit::assert(is.numeric(mu))
  testit::assert(is.numeric(k))
  testit::assert(is.numeric(num_spec) || is.null(num_spec))
  testit::assert(is.numeric(num_immigrants) || is.null(num_immigrants))
  testit::assert(is.numeric(mainland_n))

  immig_rate <- max(0, mainland_n * gam * (1 - (num_spec / k)))
  ext_rate <- mu * num_spec
  ana_rate <- laa * num_immigrants
  clado_rate <- max(0, lac * num_spec * (1 - (num_spec / k)))

  testit::assert(immig_rate >= 0.0)
  testit::assert(ext_rate >= 0.0)
  testit::assert(ana_rate >= 0.0)
  testit::assert(clado_rate >= 0.0)

  # RJCB: minor trick:
  # one can simply write
  # list(
  #   immig_rate = immig_rate,
  #   ext_rate = ext_rate,
  #   ana_rate = ana_rate,
  #   clado_rate = clado_rate
  # )
  # without the return.
  # return is great for early returns, but
  # -as far as I understand- adding it to the
  # end of the function is unnecessary
  # and makes the out be copied needlessly
  rates <- list(
    immig_rate = immig_rate,
    ext_rate = ext_rate,
    ana_rate = ana_rate,
    clado_rate = clado_rate
  )
  return(rates)
}
