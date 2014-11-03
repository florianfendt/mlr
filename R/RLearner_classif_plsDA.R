#' @export
makeRLearner.classif.plsDA = function() {
  makeRLearnerClassif(
    cl = "classif.plsDA",
    package = "DiscriMiner",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "autosel", default = TRUE),
      makeIntegerLearnerParam(id = "comps", lower = 1L, default = 2L, requires = expression(autosel==TRUE)),
      makeLogicalLearnerParam(id = "retain.models", default = FALSE)
      ),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Partial Least Squares (PLS) Discriminant Analysis",
    short.name = "plsda",
    note = ""
  )
}

#' @export
trainLearner.classif.plsDA = function(.learner, .task, .subset, .weights = NULL,  ...) {
  DiscriMiner::plsDA(variables = getTaskFeatures(.task, .subset), group = getTaskFeatures(.task, .subset), ...)
}

#' @export
predictLearner.classif.plsDA = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  p = DiscriMiner::classify(m, newdata = .newdata)
  #p$scores #we loose this information
  p$pred_class
}
