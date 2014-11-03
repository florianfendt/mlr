getTaskDesc.TaskDesc = function(task) {
  task
}

getTaskTargetNames.TaskDesc = function(task) {
  task$target
}

getTaskFormulaAsString.TaskDesc = function(task, target = getTaskTargetNames(task)) {
  sprintf("%s ~ .", target)
}
