% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_error.R
\name{calc_error}
\alias{calc_error}
\title{Calculates the colonisation through time (CTT) statistic, percentage of max
age colonists, percentage of endemics, and differences in parameter estimates
from DAISIE maximum likelihood estimation on ideal and empirical data
simulated from sim_island_with_mainland}
\usage{
calc_error(daisie_mainland_data, ideal_ml, empirical_ml)
}
\arguments{
\item{daisie_mainland_data}{\link{list} containing data of a DAISIE
simulation with mainland dynamics,
as produced by \link{sim_island_with_mainland}
and checked by \link{check_daisie_mainland_data}

A \code{daisie_mainland_data} has two elements:
\itemize{
\item \code{ideal_multi_daisie_data}: the ideal data set produced in a simulation,
see \link{check_multi_daisie_data} for more details
\item \code{empirical_multi_daisie_data}: the empirical data sets produced in a
simulation see \link{check_multi_daisie_data} for more details
}}

\item{ideal_ml}{List containing maximum likelihood estimates from DAISIE
fitted to ideal data produced from \code{sim_island_with_mainland}. Output
from \code{DAISIE::DAISIE_ML_CS}}

\item{empirical_ml}{List containing maximum likelihood estimates from DAISIE
fitted to empirical data produced from \code{sim_island_with_mainland}.
Output from \code{DAISIE::DAISIE_ML_CS}}
}
\value{
List of error metrics
}
\description{
Calculates the colonisation through time (CTT) statistic, percentage of max
age colonists, percentage of endemics, and differences in parameter estimates
from DAISIE maximum likelihood estimation on ideal and empirical data
simulated from sim_island_with_mainland
}
\author{
Joshua W. Lambert
}
