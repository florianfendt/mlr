#' Get target column(s) of a task.
#'
#' @template arg_task
#' @param drop [\code{logical}]\cr
#'   Drop unused dimensions?
#' @return [\code{data.frame} | \code{vector}] of target column(s).
#' @family Task
#' @export
getTarget = function(task, drop = TRUE) {
  UseMethod("getTarget")
}

#' Get feature column(s) of a task.
#'
#' @template arg_task
#' @template ret_task
#' @return [\code{data.frame}] of features.
#' @family Task
#' @export
getFeatures = function(task) {
  UseMethod("getFeatures")
}

#' Get names of target column(s) of a task.
#'
#' @template arg_task
#' @return [\code{character}] of target name(s).
#' @family Task
#' @export
getTargetNames = function(task) {
  UseMethod("getTargetNames")
}

#' Get names of feature column(s) of a task.
#'
#' @template arg_task
#' @return [\code{character}] of target name(s).
#' @family Task
#' @export
getFeatureNames = function(task) {
  UseMethod("getFeatureNames")
}

#' Get variable types of features.
#' @template arg_task
#' @return [\code{factor}] with levels \dQuote{numeric}, \dQuote{factor} and \dQuote{ordered},
#'  same length as feature columns.
getFeatureTypes = function(task) {
  UseMethod("getFeatureTypes")
}

#' Get number of features in task.
#'
#' @template arg_task
#' @return [\code{integer(1)}].
#' @export
#' @family task
getTaskNFeats = function(task) {
  UseMethod("getTaskNFeats")
}

#' Get formula of a task.
#'
#' This is simply \dQuote{<target> ~ .}.
#'
#' @template arg_task
#' @param target [\code{character(1)}]\cr
#'   Left hand side of formula.
#'   Default is defined by task \code{x}.
#' @param env [\code{environment}]\cr
#'   Environment of the formula. Set this to \code{parent.frame()}
#'   for the default behaviour.
#'   Default is \code{NULL} which deletes the environment.
#' @return [\code{formula}].
#' @family task
#' @export
getTaskFormula = function(task, target = getTargetNames(task), env = NULL) {
  assertCharacter(target, any.missing = FALSE)
  UseMethod("getTaskFormula")
}

#' @export
#' @rdname getTaskFormula
getTaskFormulaAsString = function(task, target = getTargetNames(task)) {
  assertCharacter(target, any.missing = FALSE)
  UseMethod("getTaskFormulaAsString")
}

#' Extract costs from task.
#'
#' Retuns \dQuote{NULL} if the task is not of type \dQuote{costsens}.
#'
#' @template arg_task
#' @return [\code{matrix} | \code{NULL}].
#' @family task
#' @export
getTaskCosts = function(task) {
  task$costs
}

#' Extract weights from task.
#'
#' Retuns \dQuote{NULL} if the task does not have weights.
#'
#' @template arg_task
#' @return [\code{numeric} | \code{NULL}].
#' @family task
#' @export
getTaskWeights = function(task) {
  task$weights
}

#' Get number of rows or columns for a task
#' @template arg_task
#' @return [integer(1)].
#' @export
getTaskRows = function(task) {
  nrow(task$data)
}

#' @rdname getTaskRows
#' @export
getTaskCols = function(task) {
  ncol(task$data)
}

#' @title Get a task description
#' @description Returns a named list summarizing a task.
#' @template arg_task
#' @return [named list].
#' @export
getTaskDesc = function(task) {
  # FIXME: we need this for some models, too
  UseMethod("getTaskDesc")
}

#' @title Recode target column to a specific format.
#' @template arg_task
#' @template subset [\code{integer} | \code{logical}]\cr
#'  Vector to optionally subset the target.
#' @template type [\code{character(1)}]\cr
#'  Options are \dQuote{01} and \dQuote{-1+1} form classification tasks and
#'  \dQuote{lcens}, \dQuote{rcens} and \dQuote{icens} for survival tasks.
#' @return [data.frame].
#' @export
recodeTarget = function(task, subset, type = "no") {
  if (identical(type, "no"))
    return (getTaskTargets(task))
  UseMethod("recodeTarget")
}

#' Get the type of a task
#' @template arg_task
#' @return [character(1)].
#' @export
getTaskType = function(task) {
  UseMethod("getTaskType")
}
