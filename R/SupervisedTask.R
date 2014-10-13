#' @param target [\code{character}]\cr
#'  Name of the target column(s) in \code{data}.
#' @rdname Task
makeSupervisedTask = function(id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL) {
  assertCharacter(target, min.len = 1L, any.missing = FALSE)
  if (!all(target %in% names(data)))
    stop("All target columns must be in 'data'")
  i = which.first(vlapply(data[target], anyMissing))
  if (length(i) > 0L)
    stopf("Target column '%s' contains missing values", target[i])
  checkTaskCreation(dropNamed(data, target), c("inf", "nan", "empty.levels"))
  task = makeTask(id = id, data = data, weights = weights, blocking = blocking)
  task$target = target
  addClasses(task, "SupervisedTask")
}
