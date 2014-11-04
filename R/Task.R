#' @title Create a classification, regression, survival, cluster, or cost-sensitive classification task.
#'
#' @description
#' The task encapsulates the data and specifies - through its subclasses -
#' the type of the task.
#'
#'
#' Object members:
#' \describe{
#' \item{env [\code{environment}]}{Environment where data for the task are stored.
#'   Use \code{\link{getTaskData}} in order to access it.}
#' \item{weights [\code{numeric}]}{See argument above. \code{NULL} if not present.}
#' \item{blocking [\code{factor}]}{See argument above. \code{NULL} if not present.}
#' \item{task.desc [\code{\link{TaskDesc}}]}{Encapsulates further information about the task.}
#' }
#'
#' @param id [\code{character(1)}]\cr
#'   Id string for object.
#'   Default is the name of R variable passed to \code{data}.
#' @param data [\code{data.frame}]\cr
#'   A data frame containing the features and target variable(s).
#' @param weights [\code{numeric}]\cr
#'   Optional, non-negative case weight vector to be used during fitting.
#'   Cannot be set for cost-sensitive learning.
#'   Default is \code{NULL} which means no (= equal) weights.
#' @param blocking [\code{factor}]\cr
#'   An optional factor of the same length as the number of observations.
#'   Observations with the same blocking level \dQuote{belong together}.
#'   Specifically, they are either put all in the training or the test set
#'   during a resampling iteration.
#'   Default is \code{NULL} which means no blocking.
#' @return [\code{\link{Task}}].
#' @name Task
#' @rdname Task
#' @family Task
#' @aliases ClassifTask RegrTask SurvTask CostSensTask ClusterTask
#' @examples
#' library(mlbench)
#' data(BostonHousing)
#' data(Ionosphere)
#'
#' makeClassifTask(data = iris, target = "Species")
#' makeRegrTask(data = BostonHousing, target = "medv")
#' # an example of a classification task with more than those standard arguments:
#' blocking = factor(c(rep(1, 51), rep(2, 300)))
#' makeClassifTask(id = "myIonosphere", data = Ionosphere, target = "Class",
#'   positive = "good", blocking = blocking)
#' makeClusterTask(data = iris[, -5L])
makeTask = function(id = as.character(deparse(substitute(data))), data, weights = NULL, blocking = NULL) {
  assertString(id)
  assertDataFrame(data, col.names = "strict")
  env = new.env(parent = emptyenv())
  env$id = id
  env$data = data

  if (!is.null(weights)) {
    assertNumeric(weights, len = nrow(data), lower = 0, any.missing = FALSE)
    env$weights = weights
  }
  if (!is.null(blocking)) {
    assertFactor(blocking, len = nrow(data), any.missing = FALSE)
    if(length(blocking) != nrow(data))
      stop("Blocking has to be of the same length as number of rows in data! Or pass none at all.")
    env$blocking = blocking
  }
  env$has.missings = anyMissing(data)
  env$type = NA_character_
  setClasses(env, "Task")
}

#' @export
print.Task = function(x, ...) {
  catf("Observations: %i", nrow(x$data))
  catf("Features:")
  catf(collapse(printToChar(table(getTaskFeatureTypes(x)), collapse = NULL)[-1L], "\n"))
  catf("Missings: %s", x$has.missings)
  catf("Has weights: %s", !is.null(x$weights))
  catf("Has blocking: %s", !is.null(x$has.blocking))
}
