#' @export
#' @rdname Task
makeRegrTask = function(id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL) {
  task = makeSupervisedTask(id, data, target, weights, blocking)
  if (length(target) != 1L)
    stop("There must be exactly one target column for regression")
  assertNumeric(data[[target]], any.missing = FALSE, finite = TRUE, .var.name = target)
  if (!is.double(data[[target]]))
    task[[target]] = as.double(data[[target]])
  task$type = "regr"
  addClasses(task, "RegrTask")
}

#' @export
getTaskDesc.RegrTask = function(task) {
  td = NextMethod("getTaskDesc")
  addClasses(td, "RegrTaskDesc")
}

#' @export
print.RegrTask = function(x, ...) {
  catf("RegrTask %s", x$id)
  catf("Target: %s", collapse(x$target))
  NextMethod("print")
}
