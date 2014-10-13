context("Task")

test_that("Task structure", {
  expect_true(setequal(ls(basic.task), c("id", "data", "props")))
  expect_true(is.character(basic.task$id))
  expect_true(is.data.frame(basic.task$data))
  expect_true(is.character(basic.task$props))
  expect_true(is.null(basic.task$weights))
  expect_true(is.null(basic.task$costs))
  expect_true(is.null(basic.task$target))
})

test_that("Task copy works", {
  task.cpy = copyTask(basic.task)

  expect_true(is.character(task.cpy$id))
  expect_true(is.data.frame(task.cpy$data))
  expect_true(is.character(task.cpy$props))
  task.cpy$id = "xxx"
  expect_true(task.cpy$id == "xxx")
  expect_true(basic.task$id == "basic.task")
})

test_that("Task operators work", {
  task = copyTask(basic.task)

  # as.data.frame
  expect_true(is.data.frame(as.data.frame(task)))
  df = as.data.frame(task)

  # [[
  expect_true(is.factor(task[["Species"]]))

  # getTaskRows, getTaskCols
  expect_equal(getTaskRows(task), nrow(df))
  expect_equal(getTaskCols(task), ncol(df))

  # [
  task = task[1:5, task = TRUE]
  expect_equal(getTaskRows(task), 5)
  expect_equal(getTaskCols(task), ncol(df))

  # [[<-
  task[["Species"]] = 1:5

  # getTargetNames
  expect_equal(getTargetNames(task), character(0))

  # getFeatures
  expect_equal(getFeatures(task), as.data.frame(task))

  # head, tail
  expect_equal(nrow(head(task, 2)), 2)
  expect_equal(nrow(tail(task, 2)), 2)

  # getTaskNFeats
  expect_equal(getTaskNFeats(task), 5L)

  # getTaskFormula
  expect_equal(class(getTaskFormula(task))[1], "formula")

  # getTaskFormulaAsString
  expect_equal(getTaskFormulaAsString(task), "~ .")
})
