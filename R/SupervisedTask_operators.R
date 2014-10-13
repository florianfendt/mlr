#' @export
getFeatures.SupervisedTask = function(task) {
  task$data[, getFeatureNames(task), drop = FALSE]
}

#' @export
getTarget.SupervisedTask = function(task, recode = "no", drop = TRUE) {
  assertString(recode)
  if (recode != "no")
    stopf("Recode option '%s' not supported by task '%s", recode, task$id)
  task$data[, task$target, drop = drop]
}

#' @export
getTargetNames.SupervisedTask = function(task) {
  task$target
}

#' @export
getFeatureNames.SupervisedTask = function(task) {
  setdiff(names(task$data), task$target)
}

#' @export
getTaskNFeats.SupervisedTask = function(task) {
  ncol(task$data) - length(task$target)
}

#' @export
getTaskDesc.SupervisedTask = function(task) {
  td = NextMethod("getTaskDesc")
  insert(td, list(target = task$target))
}
