#' @export
makeRLearner.classif.quaDA = function() {
  makeRLearnerClassif(
    cl = "classif.quaDA",
    package = "DiscriMiner",
    par.set = makeParamSet(
      #makeNumericVectorLearnerParam(id = "prior", lower = 0, upper = 1, default = NULL),
      ),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Quadratic Discriminant Analysis",
    short.name = "quada",
    note = ""
  )
}

#' @export
trainLearner.classif.quaDA = function(.learner, .task, .subset, .weights = NULL,  ...) {
  DiscriMiner::quaDA(variables = getTaskFeatures(.task, .subset), group = getTaskTarget(.task, .subset), ...)
}

#' @export
predictLearner.classif.quaDA = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  p = DiscriMiner::classify(m, newdata = .newdata)
  #p$scores #we loose this information
  p$pred_class
}
