#' Samples what event to happen next on the island.
#'
#' @inheritParams default_params_doc
#'
#' @return numeric indicating what event will happen, or a supposed event that
#' would happen in some timesteps of the ontogeny algorithm.
#' \enumerate{
#'   \item immigration event
#'   \item extinction event
#'   \item cladogenesis event
#'   \item anagenesis event
#' }
#' @keywords internal
#' @author Joshua W. Lambert
sample_event <- function(rates) {
  testit::assert(is.list(rates))
  testit::assert(is.numeric(rates$immig_rate))
  testit::assert(is.numeric(rates$ext_rate))
  testit::assert(is.numeric(rates$ana_rate))
  testit::assert(is.numeric(rates$clado_rate))
  testit::assert(rates$immig_rate >= 0.0)
  testit::assert(rates$ext_rate >= 0.0)
  testit::assert(rates$ana_rate >= 0.0)
  testit::assert(rates$clado_rate >= 0.0)

  possible_event <- sample(
    x = 1:4,
    size = 1,
    replace = FALSE,
    prob = c(
      rates$immig_rate,
      rates$ext_rate,
      rates$ana_rate,
      rates$clado_rate
    )
  )

  testit::assert(is.numeric(possible_event))
  testit::assert(possible_event >= 1)
  testit::assert(possible_event <= 4)
  return(possible_event)
}
