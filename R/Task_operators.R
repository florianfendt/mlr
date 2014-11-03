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
getTaskTargetNames.Task = function(task) {
  character(0L)
}

#' @export
getTaskFeatureNames.Task = function(task, drop = FALSE) {
  names(task$data)
}

#' @export
getTaskTarget.Task = function(task, drop = FALSE) {
  makeDataFrame(ncol = 0L, nrow = nrow(task$data), row.names = rownames(task$data))
}

#' @export
getTaskFeatures.Task = function(task) {
  task$data
}

getTaskFeatureTypes.Task = function(task) {
  if (is.null(task$feature.types))
    task$feature.types = factor(vcapply(getTaskFeatures(task), class), levels = c("numeric", "factor", "ordered"))
  task$feature.types
}

#' @export
getTaskNFeats.Task = function(task) {
  ncol(task$data)
}

#' @export
getTaskFormulaAsString.Task = function(task, target = getTaskTargetNames(task)) {
  "~ ."
}


#' @export
getTaskType.Task = function(task) {
  task$type
}

#' @export
getTaskDesc.Task = function(task) {
  makeS3Obj("TaskDesc",
    id = task$id,
    type = task$type,
    size = getTaskRows(task),
    feature.types = getTaskFeatureTypes(task),
    has.missings = task$has.missings,
    has.weights = !is.null(task$weights),
    has.blocking = !is.null(task$blocking)
  )
}
