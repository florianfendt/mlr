#' @rdname Task
#' @export
makeUnsupervisedTask = function(id = deparse(substitute(data)), data, weights = NULL, blocking = NULL) {
  checkTaskCreation(data, c("inf", "nan", "empty.levels"))
  task = makeTask(id, data, weights, blocking)
  addClasses(task, "UnsupervisedTask")
}
