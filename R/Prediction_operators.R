#' @export
as.data.frame.Prediction = function(x, row.names = NULL, optional = FALSE,...) {
  x$data
}

#' Get probabilities for some classes.
#'
#' @template arg_pred
#' @param cl [\code{character}]\cr
#'   Names of classes.
#'   Default is either all classes for multi-class problems or the positive class for binary classification.
#' @return [\code{data.frame}] with numerical columns or a numerical vector if length of \code{cl} is 1.
#'   Order of columns is defined by \code{cl}.
#' @export
#' @family predict
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn = makeLearner("classif.lda", predict.type = "prob")
#' mod = train(lrn, task)
#' # predict probabilities
#' pred = predict(mod, newdata = iris)
#'
#' # Get probabilities for all classes
#' head(getProbabilities(pred))
#'
#' # Get probabilities for a subset of classes
#' head(getProbabilities(pred, c("setosa", "virginica")))
getProbabilities = function(pred, cl) {
  assertClass(pred, classes = "Prediction")
  td = getTaskDesc(pred)
  ttype = td$type
  if (ttype %nin% c("classif", "cluster"))
    stop("Prediction was not generated from a ClassifTask or ClusterTask!")
  if (missing(cl)) {
    if (isBinaryClassifTask(pred))
      cl = td$positive
    else
      cl = td$class.levels
  } else {
    assertCharacter(cl, any.missing = FALSE)
  }
  if (pred$predict.type != "prob")
    stop("Probabilities not present in Prediction object!")
  cns = colnames(pred$data)
  if (ttype == "classif") {
    cl2 = paste("prob", cl, sep = ".")
    if (!all(cl2 %in% cns))
      stopf("Trying to get probabilities for nonexistant classes: %s", collapse(cl))
    y = pred$data[, cl2]
    if (length(cl) > 1L)
      colnames(y) = cl
  } else {
    y = pred$data[, grepl("prob\\.", cns)]
    colnames(y) = seq_col(y)
  }
  return(y)
}
