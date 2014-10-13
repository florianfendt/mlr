#' @export
"[.Task" = function(x, i, j, drop = FALSE, task = FALSE, ...) {
  if (isTRUE(task)) {
    nt = copyTask(x, ignore = "data")
    nt$data = x$data[i, j, drop = FALSE]
    if (!missing(i)) {
      nt$weights = x$weights[i]
      nt$blocking = x$blocking[i]
    }
    if (!missing(j))
      nt$feature.types = x$feature.types[j]
    return(setClasses(nt, class(x)))
  } else {
    return(x$data[i, j, drop = drop])
  }
}

#' @export
"[[.Task" = function(x, ..., exact = TRUE) {
  x$data[[..., exact = exact]]
}

#' @export
"[<-.Task" = function(x, i, j, value) {
  x$feature.types = NULL
  x$data[i, j] = value
  x
}

#' @export
"[[<-.Task" = function(x, i, j, value) {
  x$feature.types = NULL
  x$data[[i]] = value
  x
}

#' @export
as.data.frame.Task = function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x$data, row.names = row.names, optional = optional, ...)
}

#' @export
head.Task = function(x, n = 6L, ...) {
  head(as.data.frame(x), n)
}

#' @export
tail.Task = function(x, n = 6L, ...) {
  tail(as.data.frame(x), n)
}

#' @export
summary.Task = function(object, ...) {
  summary(as.data.frame(object), ...)
}

#' Copy a task.
#'
#' @template arg_task
#' @param ignore [\code{character}]\cr
#'   Items in task to ignore on copy.
#' @template ret_task
#' @family Task
#' @export
copyTask = function(task, ignore = character(0L)) {
  new.task = new.env(parent = emptyenv())
  for (item in setdiff(ls(task, all.names = TRUE), ignore)) {
    assign(item, get(item, envir = task), envir = new.task)
  }
  setClasses(new.task, class(task))
}

#' @export
getTargetNames.Task = function(task) {
  character(0L)
}

#' @export
getFeatureNames.Task = function(task, drop = FALSE) {
  names(task$data)
}

#' @export
getTarget.Task = function(task, drop = FALSE) {
  makeDataFrame(ncol = 0L, nrow = nrow(task$data), row.names = rownames(task$data))
}

#' @export
getFeatures.Task = function(task) {
  task$data
}

getFeatureTypes.Task = function(task) {
  if (is.null(task$feature.types))
    task$feature.types = factor(vcapply(getFeatures(task), class), levels = c("numeric", "factor", "ordered"))
  task$feature.types
}

#' @export
getTaskNFeats.Task = function(task) {
  ncol(task$data)
}

#' @export
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' subsetTask(task, subset = 1:100)
subsetTask = function(task, subset, features) {
  # FIXME: we recompute the taskdesc for each subsetting. do we want that? speed?
  # FIXME: maybe we want this independent of changeData?
  td = task$desc
  task = changeData(task, getTaskData(task, subset, features), getTaskCosts(task, subset), task$weights)
  if (!missing(subset)) {
    if (task$task.desc$has.blocking)
      task$blocking = task$blocking[subset]
    if (task$task.desc$has.weights)
      task$weights = task$weights[subset]
  }
  return(task)
}

#' @export
getTaskFormulaAsString.Task = function(task, target = getTargetNames(task)) {
  "~ ."
}

# we create a new env, so the reference is not changed
# FIXME: really check what goes on here! where is this called / used?
changeData = function(task, data, costs, weights) {
  if (missing(data))
    data = getTaskData(task)
  if (missing(costs))
    costs = getTaskCosts(task)
  if (missing(weights))
    weights = task$env$weights
  task$env = new.env(parent = emptyenv())
  task$env$data = data
  task$env$costs = costs
  # FIXME: I hate R
  if (is.null(weights))
    task["weights"] = list(NULL)
  else
    task$weights = weights
  td = task$task.desc
  # FIXME: this is bad style but I see no other way right now
  task$task.desc = switch(td$type,
    "classif" = makeTaskDesc(task, td$id, td$target, td$positive),
    "surv" = makeTaskDesc(task, td$id, td$target, td$censoring),
    "cluster" = makeTaskDesc(task, td$id),
    makeTaskDesc(task, td$id, td$target))
  return(task)
}

#' @export
getTaskType.Task = function(task) {
  task$type
}

#' @export
getTaskDesc.Task = function(task) {
  list(
    id = task$id,
    type = task$type,
    size = getTaskRows(task),
    feature.types = getFeatureTypes(task),
    has.missings = task$missings,
    has.weights = !is.null(task$weights),
    has.blocking = !is.null(task$blocking)
  )
}
