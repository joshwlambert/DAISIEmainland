% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_mainland.R
\name{sim_mainland}
\alias{sim_mainland}
\title{Simulates mainland extinction as a pure-death process with replacement
via the speciation of an exisiting mainland species.}
\usage{
sim_mainland(total_time, m, mainland_ex)
}
\arguments{
\item{total_time}{Numeric defining the length of the simulation in time
units.}

\item{m}{Numeric defining the size of mainland pool.}

\item{mainland_ex}{Numeric parameter for mainland extinction rate.}
}
\value{
a list where each element is a mainland lineage, this could be a
single or multiple lineages. Each element of the list is a matrix with nine
columns. The columns contain
\enumerate{
    \item Species identity
    \item Mainland ancestor identity
    \item Colonisation time
    \item Species type
    \item Branching code
    \item Branching time (forwards in time from the start of the simulation)
    \item Anagenetic origin
    \item Species origination time
    \item Species extinction time
}
}
\description{
Simulates mainland extinction as a pure-death process with replacement
via the speciation of an exisiting mainland species.
}
\keyword{internal}
