context("UnsupervisedTask")

test_that("UnsupervisedTask", {
  ct1 = noclass.task

  expect_error(makeClassifTask(data = 44), "\"target\" is missing")

  # wrong vars
  expect_error(noclass.task[, c("Sepal.Length", "x", "y")], "undefined columns selected")

  # check missing accessors
  df = noclass.df
  df[1,1:3] = NA
  df[2,1:3] = NA
  ct = makeClusterTask(data = df)
  expect_true(getTaskDesc(ct)$has.missings)

  # check that blocking is still there after subsetting
  ct1 = makeClusterTask(data = noclass.df, blocking = as.factor(1:nrow(noclass.df)))
  expect_true(getTaskDesc(ct1)$has.blocking)
  ct2 = ct1[,, task = TRUE]
  expect_true(getTaskDesc(ct2)$has.blocking)
})
