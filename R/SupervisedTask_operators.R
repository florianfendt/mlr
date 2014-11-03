#' @export
getTaskFeatures.SupervisedTask = function(task, subset) {
  task$data[subset, getTaskFeatureNames(task), drop = FALSE]
}

#' @export
getTaskTarget.SupervisedTask = function(task, subset, recode = "no", drop = TRUE) {
  assertString(recode)
  if (recode != "no")
    stopf("Recode option '%s' not supported by task '%s", recode, task$id)
  task$data[subset, task$target, drop = drop]
}

#' @export
getTaskTargetNames.SupervisedTask = function(task) {
  task$target
}

#' @export
getTaskFeatureNames.SupervisedTask = function(task) {
  setdiff(names(task$data), task$target)
}

#' @export
getTaskNFeats.SupervisedTask = function(task) {
  ncol(task$data) - length(task$target)
}

#' @export
getTaskDesc.SupervisedTask = function(task) {
  td = NextMethod("getTaskDesc")
  td = insert(td, list(target = task$target))
  addClasses(td, "SupervisedTaskDesc")
}


#' @export
getTaskFormulaAsString.SupervisedTask = function(task, target = getTaskTargetNames(task)) {
  sprintf("%s ~.", target)
}
