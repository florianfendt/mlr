#' @export
#' @rdname Task
makeClassifTask = function(id = deparse(substitute(data)), data, target,
  weights = NULL, blocking = NULL, positive = NA_character_) {
  task = makeSupervisedTask(id, data, target, weights, blocking)
  assertString(positive, na.ok = TRUE)

  if (length(target) != 1L)
    stop("There must be exactly one target column for classification")
  task[[target]] = as.factor(data[[target]])
  assertFactor(task[[target]], empty.levels.ok = FALSE, .var.name = target)
  levs = levels(task[[target]])
  if (is.na(positive)) {
    if (length(levs) == 2L) {
      positive = levs[1L]
      negative = levs[2L]
    } else {
      negative = NA
    }
  } else {
    assertChoice(positive, choices = levs)
    if (length(levs) > 2L)
      stop("Cannot set a positive class for a multiclass problem!")
    negative = levs[levs != positive]
  }

  task$type = "classif"
  task$class.levels = levs
  task$positive = positive
  task$negative = negative
  addClasses(task, "ClassifTask")
}

#' @export
getTaskDesc.ClassifTask = function(task) {
  td = NextMethod("getTaskDesc")
  td = insert(td, list(class.levels = task$class.levels, positive = task$positive, negative = task$negative))
  addClasses(td, "ClassifTaskDesc")
}

#' @export
print.ClassifTask = function(x, ...) {
  catf("ClassifTask %s", x$id)
  catf("Target: %s", collapse(x$target))
  catf("Positive class: %s", x$positive)
  NextMethod("print")
}

#' @export
recodeTaskTarget.ClassifTask = function(task, subset, type = "no") {
  assertChoice(recode, c("01", "-1+1"))
  y = task$data[subset, task$target, drop = TRUE]
  if (type == "01")
    return(as.numeric(y == task$positive))
  return(as.numeric(2L * (y == task$positive) - 1L))
}

getTaskClassLevels = function(task) {
  UseMethod("getTaskClassLevels")
}

getTaskClassLevels.default = function(task) {
  getTaskDesc(task)$class.levels
}

getTaskClassLevels.ClassifTask = function(task) {
  task$class.levels
}

isBinaryClassifTask = function(task) {
  length(getTaskClassLevels(task)) == 2L
}
