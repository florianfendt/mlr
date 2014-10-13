#' Set, add, remove or query properties of mlr objects
#'
#' @param x [\code{\link{Task}} | \code{\link{Learner}}]\cr
#'  Object with properties.
#' @param props [\code{character}]\cr
#'   Vector of properties to set, add, remove or query.
#' @return \code{setProperties}, \code{addProperties} and \code{removeProperties}
#'  return an updated object.
#'  \code{hasProperties} returns a logical vector of the same length of \code{props}.
#' @name Properties
#' @rdname Properties
NULL

#' @rdname Properties
#' @export
setProperties = function(x, props) {
  assertCharacter(props, any.missing = FALSE)
  x$properties = unique(props)
  x
}

#' @rdname Properties
#' @export
addProperties = function(x, props) {
  assertCharacter(props, any.missing = FALSE)
  x$properties = union(x$properties, props)
  x
}

#' @rdname Properties
#' @export
removeProperties = function(x, props) {
  assertCharacter(props, any.missing = FALSE)
  x$properties = setdiff(x$properties, props)
  x
}

#' @rdname Properties
#' @export
hasProperties = function(x, props) {
  assertCharacter(props, any.missing = FALSE)
  props %in% x$properties
}
