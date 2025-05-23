% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PIOutputMapping.R
\docType{class}
\name{PIOutputMapping}
\alias{PIOutputMapping}
\title{PIOutputMapping}
\description{
Establishes connections between simulated quantities and corresponding
observed data sets. Utilized within \code{ParameterIdentification} instances to align
and compare simulation outputs with empirical data.
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{observedDataSets}}{A named list containing \code{DataSet} objects for comparison
with simulation outcomes.}

\item{\code{dataTransformations}}{A named list of factors and offsets.}

\item{\code{dataWeights}}{A named list of y-value weights.}

\item{\code{quantity}}{Simulation quantities to be aligned with observed data values.}

\item{\code{simId}}{Identifier of the simulation associated with the mapped quantity.}

\item{\code{scaling}}{Specifies scaling for output mapping: linear (default) or logarithmic.}

\item{\code{transformResultsFunction}}{A function to preprocess simulated results
(time and observation values) before residual calculation. It takes
numeric vectors 'xVals' and 'yVals', and returns a named list with keys
'xVals' and 'yVals'.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PIOutputMapping-new}{\code{PIOutputMapping$new()}}
\item \href{#method-PIOutputMapping-addObservedDataSets}{\code{PIOutputMapping$addObservedDataSets()}}
\item \href{#method-PIOutputMapping-removeObservedDataSet}{\code{PIOutputMapping$removeObservedDataSet()}}
\item \href{#method-PIOutputMapping-setDataTransformations}{\code{PIOutputMapping$setDataTransformations()}}
\item \href{#method-PIOutputMapping-setDataWeights}{\code{PIOutputMapping$setDataWeights()}}
\item \href{#method-PIOutputMapping-print}{\code{PIOutputMapping$print()}}
\item \href{#method-PIOutputMapping-clone}{\code{PIOutputMapping$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PIOutputMapping-new"></a>}}
\if{latex}{\out{\hypertarget{method-PIOutputMapping-new}{}}}
\subsection{Method \code{new()}}{
Initialize a new instance of the class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PIOutputMapping$new(quantity)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{quantity}}{An object of the type \code{Quantity}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{PIOutputMapping} object.
Adds or updates observed data using \code{DataSet} objects.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PIOutputMapping-addObservedDataSets"></a>}}
\if{latex}{\out{\hypertarget{method-PIOutputMapping-addObservedDataSets}{}}}
\subsection{Method \code{addObservedDataSets()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PIOutputMapping$addObservedDataSets(data, weights = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{A \code{DataSet} object or a list thereof, matching the simulation
quantity dimensions.}

\item{\code{weights}}{A named list of numeric values or numeric vectors. The names
must match the names of the observed datasets.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Replaces any existing data set with the same label.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PIOutputMapping-removeObservedDataSet"></a>}}
\if{latex}{\out{\hypertarget{method-PIOutputMapping-removeObservedDataSet}{}}}
\subsection{Method \code{removeObservedDataSet()}}{
Removes specified observed data series.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PIOutputMapping$removeObservedDataSet(label)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{label}}{The label of the observed data series to remove.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PIOutputMapping-setDataTransformations"></a>}}
\if{latex}{\out{\hypertarget{method-PIOutputMapping-setDataTransformations}{}}}
\subsection{Method \code{setDataTransformations()}}{
Configures transformations for dataset(s).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PIOutputMapping$setDataTransformations(
  labels = NULL,
  xOffsets = 0,
  yOffsets = 0,
  xFactors = 1,
  yFactors = 1
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{labels}}{List of dataset labels for targeted transformations.
Absence of labels applies transformations globally.}

\item{\code{xOffsets}}{Numeric list/value for X-offset adjustments.}

\item{\code{yOffsets}}{Numeric list/value for Y-offset adjustments.}

\item{\code{xFactors}}{Numeric list/value for X-scaling factors.}

\item{\code{yFactors}}{Numeric list/value for Y-scaling factors.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PIOutputMapping-setDataWeights"></a>}}
\if{latex}{\out{\hypertarget{method-PIOutputMapping-setDataWeights}{}}}
\subsection{Method \code{setDataWeights()}}{
Assigns weights to observed data sets for residual weighting
during parameter identification.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PIOutputMapping$setDataWeights(weights)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{weights}}{A named list of numeric values or numeric vectors. The names
must match the names of the observed datasets.

Each element in the list can be:
\itemize{
\item a scalar, which will be broadcast to all y-values of the corresponding
dataset,
\item or a numeric vector matching the number of y-values for that dataset.
}

To apply both dataset-level and point-level weights, multiply them beforehand
and provide the combined result as a single numeric vector per dataset.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PIOutputMapping-print"></a>}}
\if{latex}{\out{\hypertarget{method-PIOutputMapping-print}{}}}
\subsection{Method \code{print()}}{
Prints a summary of the PIOutputMapping.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PIOutputMapping$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PIOutputMapping-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PIOutputMapping-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PIOutputMapping$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
