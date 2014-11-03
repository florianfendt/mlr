#' @rdname Task
#' @param censoring [\code{character(1)}]\cr
#'  Censoring type. Allowed choices are \dQuote{rcens} for right censored data (default),
#'  \dQuote{lcens} for left censored and \dQuote{icens} for interval censored data using
#'  the \dQuote{interval2} format.
#'  See \code{\link[survival]{Surv}} for details.
#' @export
makeSurvTask = function(id = deparse(substitute(data)), data, target, censoring = "rcens", weights = NULL, blocking = NULL) {
  task = makeSupervisedTask(id, data, target, weights, blocking)
  assertChoice(censoring, choices = c("rcens", "lcens", "icens"))

  time1 = data[[target[1L]]]
  time2 = data[[target[2L]]]
  if (censoring == "icens") {
    assertNumeric(time1, any.missing = TRUE, finite = FALSE, .var.name = "target column time1")
    assertNumeric(time2, any.missing = TRUE, finite = FALSE, .var.name = "target column time2")
    if (is.integer(time1))
      task$data[[target[1L]]] = as.double(time1)
    if (is.integer(time2))
      task$data[[target[2L]]] = as.double(time2)
  } else {
    assertNumeric(time1, lower = 0, finite = TRUE, any.missing = FALSE, .var.name = "target column time")
    assertAtomicVector(time2, any.missing = FALSE, .var.name = "target column event")
    if (is.integer(time1))
      task$data[[target[1L]]] = as.double(time1)

    ok = FALSE
    if (is.logical(time2)) {
      ok = TRUE
    } else if (is.numeric(time2)) {
      if (testIntegerish(time2) && all(as.integer(time2) %in% c(0L, 1L))) {
        task$env$data[[target[2L]]] = (as.integer(time2) == 1L)
        ok = TRUE
      }
    } else if (is.factor(time2)) {
      lvls = levels(time2)
      if (length(lvls) == 2L) {
        if (all(lvls %in% c("FALSE", "TRUE"))) {
          task$env$data[[target[2L]]] = (time2 == "TRUE")
          ok = TRUE
        } else if (all(lvls %in% c("0", "1"))) {
          task$env$data[[target[2L]]] = (time2 == "1")
          ok = TRUE
        }
      }
    }
    if (!ok)
      stop("Event column must be binary (logical, integer {0,1} or factor {FALSE,TRUE}/{0/1}")
  }
  task$type = "surv"
  task$censoring = censoring
  addClasses(task, "SurvTask")
}

getTaskDesc.SurvTask = function(task) {
  td = NextMethod("getTaskDesc")
  td = insert(td, list(censoring = task$censoring))
  addClasses(td, "SurvTaskDesc")
}

#' @export
print.SurvTask = function(x, ...) {
  catf("SurvTask %s", x$id)
  catf("Target: %s", collapse(x$target))
  catf("Censoring: %s", x$censoring)
  NextMethod("print")
}

#' @export
recodeTarget.SurvTask = function(task, subset, recode = "no") {
  assertChoice(recode, c("lcens", "rcens", "icens"))

  y = getTaskTarget(task)[subset,, drop = FALSE]
  from = task$censoring
  lookup = setNames(c("left", "right", "interval2"), c("lcens", "rcens", "icens"))

  if (from == recode)
    return(Surv(y[, 1L], y[, 2L], type = lookup[recode]))

  if (setequal(c(from, recode), c("lcens", "rcens")))
    stop("Converting left censored to right censored data (or vice versa) is not possible")

  if (from == "rcens") {
    time1 = y[, 1L]
    time2 = ifelse(y[, 2L], y[, 1L], Inf)
  } else if (from == "lcens") {
    time1 = ifelse(y[, 2L], y[, 1L], -Inf)
    time2 = y[, 1L]
  } else {
    if (recode == "lcens") {
      is.neg.infinite = function(x) is.infinite(x) & x < 0
      if (!all(is.neg.infinite(y[, 1L] | y[, 1L] == y[, 2L])))
        stop("Could not convert interval2 survival data to left censored data")
      time1 = y[, 2L]
      time2 = is.infinite(y[, 1L])
    } else {
      is.pos.infinite = function(x) is.infinite(x) & x > 0
      if (!all(is.pos.infinite(y[, 2L] | y[, 2L] == y[, 1L])))
        stop("Could not convert interval2 survival data to right censored data")
      time1 = y[, 1L]
      time2 = is.infinite(y[, 2L])
    }
  }
  Surv(time1, time2, type = lookup[recode])
}
