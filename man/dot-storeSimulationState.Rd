% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-simulation.R
\name{.storeSimulationState}
\alias{.storeSimulationState}
\title{Stores current simulation output state}
\usage{
.storeSimulationState(simulations)
}
\arguments{
\item{simulations}{List of \code{Simulation} objects}
}
\value{
A named list with entries \code{outputIntervals}, \code{timePoints}, and
\code{outputSelections}. Every entry is a named list with names being the IDs
of the simulations.
}
\description{
Stores simulation output intervals, output time points,
and output selections in the current state.
}
\keyword{internal}
