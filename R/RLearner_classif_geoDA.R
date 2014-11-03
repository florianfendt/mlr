#' @export
makeRLearner.classif.geoDA = function() {
  makeRLearnerClassif(
    cl = "classif.geoDA",
    package = "DiscriMiner",
    par.set = makeParamSet(),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Geometric Predictive Discriminant Analysis",
    short.name = "geoda",
    note = ""
  )
}

#' @export
trainLearner.classif.geoDA = function(.learner, .task, .subset, .weights = NULL,  ...) {
  DiscriMiner::geoDA(variables = getTaskFeatures(.task, .subset), group = getTaskTarget(.task, .subset), ...)
}

#' @export
predictLearner.classif.geoDA = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  p = DiscriMiner::classify(m, newdata = .newdata)
  #p$scores #we loose this information
  p$pred_class
}
