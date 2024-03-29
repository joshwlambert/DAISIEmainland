% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_endemic_percent.R
\name{calc_endemic_percent}
\alias{calc_endemic_percent}
\title{Calculates what percentage of island species are endemic.}
\usage{
calc_endemic_percent(daisie_mainland_data)
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
}
\value{
A list of six numeric vectors
}
\description{
Calculates what percentage of island species are endemic.
}
\author{
Joshua W. Lambert
}
