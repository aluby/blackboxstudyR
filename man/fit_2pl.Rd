% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit-2pl.R
\name{fit_2pl}
\alias{fit_2pl}
\title{Fit 2PL Model}
\usage{
fit_2pl(irt_data, iterations = 600, n_chains = 4)
}
\arguments{
\item{irt_data}{List formatted as in irt_data_bb()}

\item{iterations}{Number of MCMC iterations per chain}

\item{n_chains}{Number of MCMC chains to run}
}
\value{
stan object
}
\description{
Fit 2PL Model
}
\examples{
\dontrun{
fit_2pl()
}
}
