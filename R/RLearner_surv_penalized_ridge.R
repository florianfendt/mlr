#' @export
makeRLearner.surv.penalized.ridge = function() {
  makeRLearnerSurv(
    cl = "surv.penalized.ridge",
    package = "!penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda2", default = 0, lower = 0),
      makeUntypedLearnerParam(id = "unpenalized"),
      makeLogicalVectorLearnerParam(id = "positive", default = FALSE),
      makeNumericVectorLearnerParam(id = "startbeta"),
      makeNumericVectorLearnerParam(id = "startgamma"),
      # untyped here because one can also pass "Park" to steps
      makeUntypedLearnerParam(id = "steps", default = 1L),
      makeNumericLearnerParam(id = "epsilon", lower = 0, default = 1e-10),
      makeIntegerLearnerParam(id = "maxiter", lower = 1L),
      makeLogicalLearnerParam(id = "standardize", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(trace = FALSE),
    properties = c("numerics", "factors", "ordered", "rcens"),
    name = "Ridge Regression",
    short.name = "ridge",
    note = "trace=FALSE was set by default to disable logging output."
  )
}

#' @export
trainLearner.surv.penalized.ridge = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  penalized::penalized(f, data = getTaskData(.task, subset = .subset),
    model = "cox", fusedl = FALSE, ...)
}

#' @export
predictLearner.surv.penalized.ridge = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response") {
    # Note: this is a rather ugly hack but should work according to Jelle
    penalized::survival(penalized::predict(.model$learner.model, data = .newdata), Inf)
  } else {
    stop("Unknown predict type")
  }
}
