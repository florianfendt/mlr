% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotBMRSummary.R
\name{plotBMRSummary}
\alias{plotBMRSummary}
\title{Plot a benchmark summary.}
\usage{
plotBMRSummary(bmr, measure = NULL, trafo = "none", order.tsks = NULL,
  pointsize = 4L, jitter = 0.05, pretty.names = TRUE)
}
\arguments{
\item{bmr}{[\code{\link{BenchmarkResult}}]\cr
Benchmark result.}

\item{measure}{[\code{\link{Measure}}]\cr
Performance measure.
Default is the first measure used in the benchmark experiment.}

\item{trafo}{[\code{character(1)}]\cr
Currently either \dQuote{none} or \dQuote{rank}, the latter performing a rank transformation
(with average handling of ties) of the scores per task.
NB: You can add always add \code{\link[ggplot2]{scale_x_log10}} to the result to put scores on a log scale.
Default is \dQuote{none}.}

\item{order.tsks}{[\code{character(n.tasks)}] \cr 
Character vector with \code{task.ids} in new order.}

\item{pointsize}{[\code{numeric(1)}]\cr
Point size for ggplot2 \code{\link[ggplot2]{geom_point}} for data points.
Default is 4.}

\item{jitter}{[\code{numeric(1)}]\cr
Small vertical jitter to deal with overplotting in case of equal scores.
Default is 0.05.}

\item{pretty.names}{[\code{logical{1}}]\cr
Whether to use the short name of the learner instead of its ID in labels. Defaults to \code{TRUE}.}
}
\value{
ggplot2 plot object.
}
\description{
Creates a scatter plot, where each line refers to a task.
On that line the aggregated scores for all learners are plotted, for that task.
Optionally, you can apply a rank transformation or just use one of ggplot2's transformations
like \code{\link[ggplot2]{scale_x_log10}}.
}
\examples{
# see benchmark
}
\seealso{
Other benchmark: \code{\link{BenchmarkResult}},
  \code{\link{benchmark}},
  \code{\link{convertBMRToRankMatrix}},
  \code{\link{friedmanPostHocTestBMR}},
  \code{\link{friedmanTestBMR}},
  \code{\link{generateCritDifferencesData}},
  \code{\link{getBMRAggrPerformances}},
  \code{\link{getBMRFeatSelResults}},
  \code{\link{getBMRFilteredFeatures}},
  \code{\link{getBMRLearnerIds}},
  \code{\link{getBMRLearnerShortNames}},
  \code{\link{getBMRLearners}},
  \code{\link{getBMRMeasureIds}},
  \code{\link{getBMRMeasures}}, \code{\link{getBMRModels}},
  \code{\link{getBMRPerformances}},
  \code{\link{getBMRPredictions}},
  \code{\link{getBMRTaskIds}},
  \code{\link{getBMRTuneResults}},
  \code{\link{plotBMRBoxplots}},
  \code{\link{plotBMRRanksAsBarChart}},
  \code{\link{plotCritDifferences}}

Other plot: \code{\link{plotBMRBoxplots}},
  \code{\link{plotBMRRanksAsBarChart}},
  \code{\link{plotCalibration}},
  \code{\link{plotCritDifferences}},
  \code{\link{plotFilterValuesGGVIS}},
  \code{\link{plotFilterValues}},
  \code{\link{plotLearningCurveGGVIS}},
  \code{\link{plotLearningCurve}},
  \code{\link{plotPartialDependenceGGVIS}},
  \code{\link{plotPartialDependence}},
  \code{\link{plotROCCurves}},
  \code{\link{plotThreshVsPerfGGVIS}},
  \code{\link{plotThreshVsPerf}}
}

