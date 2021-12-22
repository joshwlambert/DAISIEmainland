% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/daisie_data_to_tables.R
\name{daisie_data_to_tables}
\alias{daisie_data_to_tables}
\title{Convert an \code{daisie_data} to a list of tables}
\usage{
daisie_data_to_tables(daisie_data)
}
\arguments{
\item{daisie_data}{List containing data of DAISIE simulation with mainland
dynamics. Output from \code{sim_island_with_mainland}.

A list. The highest level of the list has two elements called
\code{ideal_islands} and \code{empirical_islands} which corresponds to
the ideal and empirical data sets produce in each simulation. Within each
\code{ideal_islands} and \code{empirical_islands} each element is an
individual replicate. The first element of each replicate is composed of
island information containing:
\describe{
\item{\verb{$island_age}}{A numeric with the island age.}
\item{\verb{$not_present}}{the number of mainland lineages that are not
present on the island.}
}
The subsequent elements of the list pertaining to each replicate contain
information on a single colonist lineage on the island and have 3 components:
\describe{
\item{\verb{$branching_times}}{island age and stem age of the
population/species in the case of Non-endemic, Non-endemic_MaxAge and
Endemic anagenetic species.
For cladogenetic species these should
be island age and branching times of the radiation including the
stem age of the radiation.}
\item{\verb{$stac}}{An integer ranging from 1 to 6
indicating the status of the colonist:
\enumerate{
\item Non_endemic_MaxAge
\item Endemic
\item Endemic&Non_Endemic
\item Non_endemic_MaxAge
\item Endemic_singleton_MaxAge
\item Endemic_clade_MaxAge
}}
\item{\code{missing_species}}{number of island species that were
not sampled for particular clade (only applicable for endemic clades)}
}}
}
\value{
a \link{list} with elements:
\itemize{
\item \code{ideal_island_table}: the \code{daisie_data$empirical_islands} as a table,
as done by \link{empirical_daisie_data_to_tables}
\item \code{empirical_island_table}: the \code{daisie_data$ideal_island} as a table
as done by \link{ideal_daisie_data_to_tables}
}
}
\description{
Convert an \code{daisie_data} to a list of tables
}
\examples{
set.seed(
  4,
  kind = "Mersenne-Twister",
  normal.kind = "Inversion",
  sample.kind = "Rejection"
)
daisie_data <- sim_island_with_mainland(
  total_time = 1.0,
  m = 10,
  island_pars = c(1, 1, 10, 0.1, 1),
  mainland_ex = 1,
  mainland_sample_prob = 1,
  mainland_sample_type = "complete",
  replicates = 1)
daisie_data_to_tables(daisie_data)

}
\author{
Richèl J.C. Bilderbeek
}