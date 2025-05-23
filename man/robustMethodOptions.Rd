% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enums.R
\docType{data}
\name{robustMethodOptions}
\alias{robustMethodOptions}
\title{Robust Weighting Methods for Cost Function}
\format{
An object of class \code{list} of length 3.
}
\usage{
robustMethodOptions
}
\description{
Robust weighting methods to address outliers in the cost function calculation
during parameter optimization.
}
\details{
The available methods are:
\describe{
\item{\code{"none"}}{No robust weighting applied.}
\item{\code{"huber"}}{Huber weighting for moderate outliers.}
\item{\code{"bisquare"}}{Bisquare (Tukey's biweight) weighting for severe outliers.}
}
}
\keyword{datasets}
