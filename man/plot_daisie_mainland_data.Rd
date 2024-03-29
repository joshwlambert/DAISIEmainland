% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_daisie_mainland_data.R
\name{plot_daisie_mainland_data}
\alias{plot_daisie_mainland_data}
\title{Plot the \code{daisie_mainland_data} for a certain replicate}
\usage{
plot_daisie_mainland_data(daisie_mainland_data, replicate_index)
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

\item{replicate_index}{index of the replicate}
}
\value{
a \code{ggplot2}
}
\description{
Plot the \code{daisie_mainland_data} for a certain replicate
}
\examples{
daisie_mainland_data <- sim_island_with_mainland(
  total_time = 1,
  m = 10,
  island_pars = c(1, 1, 10, 0.1, 1),
  mainland_ex = 1,
  mainland_sample_prob = 1,
  mainland_sample_type = "undiscovered",
  replicates = 10,
  verbose = FALSE
)

plot_daisie_mainland_data(
  daisie_mainland_data,
  replicate_index = 1
)
}
\author{
Richèl J.C. Bilderbeek
}
