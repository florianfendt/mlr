# FIXME: parset
#' @export
makeRLearner.classif.lvq1 = function() {
  makeRLearnerClassif(
    cl = "classif.lvq1",
    package = "class",
    par.set = makeParamSet(),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Learning Vector Quantization",
    short.name = "lvq1",
    note = ""
  )
}

#' @export
trainLearner.classif.lvq1 = function(.learner, .task, .subset, .weights = NULL,  ...) {
  target = getTaskTarget(.task, .subset)
  data = getTaskFeatures(.task, .subset)
  cdbk.args = insert(list(), list(...), c("size", "k", "prior"))
  cdbk.args$cl = target
  cdbk.args$x = data
  codebk = do.call(class::lvqinit, cdbk.args)

  lvq.args = insert(list(), list(...), c("niter", "alpha"))
  lvq.args$cl = target
  lvq.args$x = data
  lvq.args$codebk = codebk
  do.call(class::lvq1, lvq.args)
}

#' @export
predictLearner.classif.lvq1 = function(.learner, .model, .newdata, ...) {
  class::lvqtest(.model$learner.model, test = .newdata, ...)
}
