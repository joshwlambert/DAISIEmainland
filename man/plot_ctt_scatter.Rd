% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_ctt_scatter.R
\name{plot_ctt_scatter}
\alias{plot_ctt_scatter}
\title{Plots points of the mean delta colonisation through time statistic (y-axis)
for either mainland extinction rate or mainland sampling probability
(x-axis).}
\usage{
plot_ctt_scatter(analysis_results, output_file_path, parameter)
}
\arguments{
\item{analysis_results}{A list of \code{analysis_result} elements which can be
checked with \code{check_analysis_result}}

\item{output_file_path}{String specifying the directory the file is saved
in, if NULL the file is returned to console and not saved}

\item{parameter}{String specifying which parameter is plotted.
"mainland_ex" plots mainland extinction as the variable,
"mainland_sample_prob" plots mainland sampling probability as the variable
and "both" plots all the data}
}
\value{
Void (saves plot)
}
\description{
Plots points of the mean delta colonisation through time statistic (y-axis)
for either mainland extinction rate or mainland sampling probability
(x-axis).
}
