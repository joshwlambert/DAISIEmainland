% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_multi_daisie_data.R
\name{group_multi_daisie_data}
\alias{group_multi_daisie_data}
\title{Takes mixed multi_daisie_data produced by the looping over \code{sim_island} in
\code{sim_island_with_mainland} and groups the ideal and the empirical
\code{multi_daisie_data} together}
\usage{
group_multi_daisie_data(multi_daisie_data, total_time, m)
}
\arguments{
\item{multi_daisie_data}{A list of \code{daisie_data} elements}

\item{total_time}{Numeric defining the length of the simulation in time
units.}

\item{m}{Numeric defining the size of mainland pool.}
}
\value{
A list of two \code{multi_daisie_data} elements
}
\description{
Takes mixed multi_daisie_data produced by the looping over \code{sim_island} in
\code{sim_island_with_mainland} and groups the ideal and the empirical
\code{multi_daisie_data} together
}
