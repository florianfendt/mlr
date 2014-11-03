#' @export
makeRLearner.classif.logreg = function() {
  makeRLearnerClassif(
    cl = "classif.logreg",
    package = "stats",
    par.set = makeParamSet(),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "Logistic Regression",
    short.name = "logreg",
    note = ""
  )
}

#' @export
trainLearner.classif.logreg = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  stats::glm(f, data = .task[.subset, ], model = FALSE, family = "binomial", ...)
}

#' @export
predictLearner.classif.logreg = function(.learner, .model, .newdata, ...) {
  x = predict(.model$learner.model, newdata = .newdata, type = "response", ...)
  levs = getTaskClassLevels(.model)
  if (.learner$predict.type == "prob") {
    # FIXME: this should be a helper function
    y = matrix(0, ncol = 2L, nrow = nrow(.newdata))
    colnames(y) = levs
    y[, 1L] = 1-x
    y[, 2L] = x
    return(y)
  } else {
    p = as.factor(ifelse(x > 0.5, levs[2L], levs[1L]))
    unname(p)
  }
}
