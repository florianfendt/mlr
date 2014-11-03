#' @export
makeRLearner.classif.knn = function() {
  makeRLearnerClassif(
    cl = "classif.knn",
    package = "class",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "k", default = 1L, lower = 1L),
      makeNumericLearnerParam(id = "l", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "use.all", default = TRUE)
    ),
    # knn cannot really return probs, only for the winning class (yeah well done BR)
    # knn also cannot handle factors in features apparantly
    properties = c("twoclass", "multiclass", "numerics"),
    name = "k-Nearest Neighbor",
    short.name = "knn",
    note = ""
  )
}

#' @export
trainLearner.classif.knn = function(.learner, .task, .subset, .weights = NULL,  ...) {
  c(list(train = getTaskFeatures(.task, .subset), cl = getTaskTarget(.task, .subset)), list(...))
}

#' @export
predictLearner.classif.knn = function(.learner, .model, .newdata, ...) {
  args = .model$learner.model
  args$test = .newdata
  do.call(class::knn, args)
}

