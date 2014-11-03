#FIXME: probs can only be predicted for two class problems (winning class)
#' @export
makeRLearner.classif.fnn = function() {
  makeRLearnerClassif(
    cl = "classif.fnn",
    package = "FNN",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "k", default = 1L, lower = 1L),
      makeLogicalLearnerParam(id = "use.all", default = TRUE, requires = expression(algorithm == "VR")),
      makeDiscreteLearnerParam(id = "algorithm", default = "cover_tree", values = list("cover_tree", "kd_tree", "VR"))
    ),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Fast k-Nearest Neighbour",
    short.name = "fnn",
    note = ""
  )
}

#' @export
trainLearner.classif.fnn = function(.learner, .task, .subset, .weights = NULL,  ...) {
  list(
    target = getTaskTarget(.task, .subset),
    data = getTaskFeatures(.task, .subset),
    parset = list(...)
  )
}

#' @export
predictLearner.classif.fnn = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  print(str(m))
  pars = list(train = m$data, test = .newdata, cl = m$target)
  pars = c(pars, m$parset, list(...))
  p = do.call(FNN::knn, pars)
  attr(p, "nn.index") = NULL
  attr(p, "nn.dist") = NULL
  return(p)
}
