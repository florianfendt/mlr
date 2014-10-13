#' @param costs [\code{data.frame}]\cr
#'   A numeric matrix or data frame containing the costs of misclassification.
#'   We assume the general case of observation specific costs.
#'   This means we have n rows, corresponding to the observations, in the same order as \code{data}.
#'   The columns correspond to classes and their names are the class labels
#'   (if unnamed we use y1 to yk as labels).
#'   Each entry (i,j) of the matrix specifies the cost of predicting class j
#'   for observation i.
#' @export
#' @rdname Task
#' @family costsens
makeCostSensTask = function(id = deparse(substitute(data)), data, costs, blocking = NULL) {
  task = makeTask(id, data, weights = NULL, blocking = blocking)
  assert(checkMatrix(costs, any.missing = FALSE), checkDataFrame(costs, any.missing = FALSE))

  if (is.data.frame(costs))
    costs = as.matrix(costs)
  assertNumeric(costs, lower = 0)
  if (is.null(colnames(costs)))
    colnames(costs) = paste0("y", seq_col(costs))

  if (nrow(costs) != nrow(data))
    stopf("Number of rows in cost matrix (%i) should equal the number of observations (%i).",
      nrow(task$costs), nrow(task$data))
  # we use ..y.. later in the models as a name for temp labels
  if ("..y.." %in% c(colnames(data), colnames(costs)))
    stopf("The name '..y..' is currently reserved for costsens tasks. You can use it neither for features nor labels!")

  task$type = "costsens"
  task$costs = costs
  task$class.levels = colnames(costs)
  addClasses(task, "CostSensTask")
}

#' @export
getTaskDesc.CostSensTask = function(task) {
  td = NextMethod("getTaskDesc")
  insert(td, list(class.levels = task$class.levels))
}

#' @export
print.CostSensTask = function(x, ...) {
  catf("CostSensTask %s", x$id)
  catf("Target: %s", collapse(x$target))
  catf("Censoring: %s", x$censoring)
  NextMethod("print")
}
