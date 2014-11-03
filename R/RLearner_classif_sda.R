#' @export
makeRLearner.classif.sda = function() {
  makeRLearnerClassif(
    cl = "classif.sda",
    package = "sda",
    par.set = makeParamSet(
      makeNumericLearnerParam("lambda", lower = 0, upper = 1),
      makeNumericLearnerParam("lambda.var", lower = 0, upper = 1),
      makeNumericLearnerParam("lambda.freqs", lower = 0, upper = 1),
      makeLogicalLearnerParam("diagonal", default = FALSE),
      makeLogicalLearnerParam("verbose", default = TRUE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "prob"),
    name = "Shrinkage Discriminant Analysis",
    short.name = "sda",
    note = ""
  )
}

#' @export
trainLearner.classif.sda = function(.learner, .task, .subset,  ...) {
  sda::sda(
    Xtrain = as.matrix(getTaskFeatures(.task, .subset)),
    L = getTaskTarget(.task, .subset), ...)
}

#' @export
predictLearner.classif.sda = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, as.matrix(.newdata))
  if(.learner$predict.type == "response")
    return(p$class)
  else
    return(p$posterior)
}
