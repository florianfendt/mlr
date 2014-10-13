checkTaskCreation = function(data, checks) {
  funs = list(
    inf = function(cn, x) {
      if (any(is.infinite(x)))
        stopf("Column '%s' contains infinite values.", cn)
    },
    nan = function(cn, x) {
      if (any(is.nan(x)))
        stopf("Column '%s' contains NaN values.", cn)
    },
    empty.levels = function(cn, x) {
      if (any(table(x) == 0L))
        stopf("Column '%s' contains empty factor levels.", cn)
    }
  )

  for(fun in funs[checks])
    Map(fun, cn = names(data), x = data)
}
