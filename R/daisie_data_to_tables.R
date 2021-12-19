#' Convert an `daisie_data` to a list of tables
#'
#' @inheritParams default_params_doc
#'
#' @return a \link{list} with elements:
#'   * `speciations`: a table with the speciation events
#'   * `colonisations`: a table with colonisations
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
daisie_data_to_tables <- function(daisie_data) {

  tables <- list()
  tables$empirical_island <- empirical_daisie_data_to_tables(
    empirical_daisie_data = daisie_data$empirical_islands
  )
  tables$empirical_island$speciations$data_type <- "empirical"
  if (nrow(tables$empirical_island$colonisations) != 0) {
    tables$empirical_island$colonisations$data_type <- "empirical"
  }
  tables$ideal_island <- ideal_island_to_tables(island$ideal_island)
  tables$ideal_island$speciations$data_type <- "ideal"
  if (nrow(tables$ideal_island$colonisations) != 0) {
    tables$ideal_island$colonisations$data_type <- "ideal"
  }
  tables$speciations <- dplyr::bind_rows(
    tables$empirical_island$speciations,
    tables$ideal_island$speciations
  )
  tables$colonisations <- dplyr::bind_rows(
    tables$empirical_island$colonisations,
    tables$ideal_island$colonisations
  )
  tables
}

#' Convert an `empirical_island` to a list of tables
#' @inheritParams default_params_doc
#' @return a \link{list} with elements:
#'   * `speciations`: a table with the speciation events
#'   * `colonisations`: a table with colonisations
#' @export
empirical_daisie_data_to_tables <- function(empirical_daisie_data) {
  # It is exactly the same
  DAISIEmainland::ideal_daisie_data_to_tables(
    ideal_daisie_data = empirical_daisie_data
  )
}

#' Convert an `ideal_island` to a list of tables
#' @inheritParams default_params_doc
#' @return a \link{list} with elements:
#'   * `speciations`: a table with the speciation events
#'   * `colonisations`: a table with colonisations
#' @export
ideal_daisie_data_to_tables <- function(ideal_daisie_data) {
  testthat::expect_true(is.list(ideal_daisie_data))
  n_replicates <- length(ideal_daisie_data)

  ideal_daisie_data[[1]]
  ideal_daisie_data[[1]][[1]]
  ideal_daisie_data[[1]][[2]]
  ideal_daisie_data[[1]][[3]]
  DAISIE::DAISIE_ML_CS(
    datalist = ideal_daisie_data[[1]],
    initparsopt = c(1, 1, 50, 0.1, 1),
    idparsopt = 1:5,
    parsfix = NULL,
    idparsfix = NULL,
    ddmodel = 11,
    methode = "odeint::runge_kutta_fehlberg78",
    optimmethod = "simplex",
    jitter = 1e-5)

  DAISIE::DAISIE_ML(
    datalist = ideal_daisie_data,
    initparsopt = c(2.5,2.7,20,0.009,1.01),
    ddmodel = 11,
    idparsopt = 1:5,
    parsfix = NULL,
    idparsfix = NULL
  )
  pars1 = c(0.195442017,0.087959583,Inf,0.002247364,0.873605049,
            3755.202241,8.909285094,14.99999923,0.002247364,0.873605049,0.163)
  pars2 = c(100,11,0,1)
  DAISIE::DAISIE_loglik_all(pars1,pars2,ideal_daisie_data)



  # It is always the same?
  testthat::expect_true(
    length(ideal_daisie_data) == 2 &&
      length(ideal_daisie_data[[1]]) == 1 &&
      length(ideal_daisie_data[[1]][[1]]) == 2 &&
      ideal_daisie_data[[1]][[1]]$island_age == total_time &&
      ideal_daisie_data[[1]][[1]]$not_present == 0 &&
      length(ideal_daisie_data[[2]]) == 1 &&
      length(ideal_daisie_data[[2]][[1]]) == 2 &&
      ideal_daisie_data[[2]][[1]]$island_age == total_time &&
      ideal_daisie_data[[2]][[1]]$not_present == 0
  )
  stop("No idea how to put this in a table yet")
}
