% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_summary.R
\name{plot_summary}
\alias{plot_summary}
\title{PLOT SUMMARY}
\usage{
plot_summary(pt, id_var)
}
\arguments{
\item{pt}{A data frame consisting of the purchase variables for each `id_var`}

\item{id_var}{The name of the unique identifier (ID) as found in the data frame.}
}
\description{
This function helps users to inspect the price-level purchase task data by
visualizing the mean and standard error of both consumption and expenditure.
}
\examples{
\dontrun{
plot_summary(pt, id_var = "ID")
}
}
