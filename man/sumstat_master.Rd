% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sumstat_master.R
\name{sumstat_master}
\alias{sumstat_master}
\title{Gather Summary Statistics from Regression Results for Output}
\usage{
sumstat_master(
  regs,
  sumstat_include = c("nobs", "adj.r.squared", "Ymean"),
  sumstat_format,
  cluster_names
)
}
\arguments{
\item{regs}{stored regression output in a list}

\item{sumstat_include}{vector of summary statistics to include}

\item{sumstat_format}{tibble from the lookup with summary stats and proper names}

\item{cluster_names}{named vector of cluster SE variables}
}
\description{
Helper function to construct summary statistics
}
\keyword{internal}
