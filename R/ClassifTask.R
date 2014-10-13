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
  if (!is.na(positive)) {
    assertChoice(positive, choices = levs)
    if (length(levs) > 2L)
      stop("Cannot set a positive class for a multiclass problem!")
  }

  task$type = "clasif"
  task$class.levels = levs
  task$positive = positive
  addClasses(task, "ClassifTask")
}

#' @export
getTaskDesc.ClassifTask = function(task) {
  td = NextMethod("getTaskDesc")
  insert(td, list(class.levels = task$class.levels, positive = task$positive))
}

#' @export
print.ClassifTask = function(x, ...) {
  catf("ClassifTask %s", x$id)
  catf("Target: %s", collapse(x$target))
  catf("Positive class: %s", x$positive)
  NextMethod("print")
}

#' @export
recodeTarget.ClassifTask = function(task, subset, recode = "no") {
  assertChoice(recode, c("01", "-1+1"))
  y = task$data[subset, task$target, drop = TRUE]
  if (type == "01")
    return(as.numeric(y == task$positive))
  return(as.numeric(2L * (y == task$positive) - 1L))
}
