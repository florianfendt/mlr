#' @rdname Task
#' @export
makeClusterTask = function(id = deparse(substitute(data)), data, weights = NULL, blocking = NULL) {
  task = makeUnsupervisedTask(id, data, weights, blocking)
  task$type = "cluster"
  addClasses(task, "ClusterTask")
}

#' @export
print.ClusterTask = function(x, ...) {
  catf("ClusterTask %s", x$id)
  NextMethod("print")
}
