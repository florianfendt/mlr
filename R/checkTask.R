checkTask = function(task, cl = "SupervisedTask", binary = FALSE) {
  assertClass(task, classes = cl)
  td = getTaskDesc(task)
  if (binary && length(td$class.levels) != 2L)
    stopf("Task '%s' must be binary classification!", td$id)
}
