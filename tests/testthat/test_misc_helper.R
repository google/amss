# Copyright 2017 Google Inc. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

context("tests for miscellaneous helper functions")

test_that("HillTrans functions correctly on both vectors and matrices", {
  expect_equal(HillTrans(1:9, ec = 4, slope = 4, beta = 2),
               HillTrans(1:9, ec = 4, slope = 4) * 2)
  expect_equal(
      HillTrans(matrix(1:6, 3),
                        ec = c(100, 40), slope = c(4, 6)),
      cbind(HillTrans(matrix(1:3), ec = 100, slope = 4),
            HillTrans(matrix(4:6), ec = 40, slope = 6)))
})


context(".PasteD")

test_that("strings concatenate correctly", {
  expect_identical(.PasteD('tmp', 'var'), 'tmp.var')
})


context(".ParseT, .EvalText")

test_that("shortcutting parse(text = ...) works inside data.table", {
  test.dt <- data.table(col.1 = 1:5, col.2 = 11:15)
  for (iter.col in 1:2) {
    colname <- .PasteD("col", iter.col)
    expect_equal(test.dt[, eval(.ParseT(colname))],
                     10 * (iter.col - 1) + 1:5)
  }
  test.dt[,
          (.PasteD(colname, "plus1")) :=
              eval(.ParseT(colname)) + 1]
  expect_equal(test.dt[, col.2.plus1], 12:16)
  # only a subset of rows
  expect_equal(2, test.dt[col.1 == 2, eval(.ParseT("col.1"))])
})

test_that("evaluation shortcut works inside and outside of data.tables", {
  test.dt <- data.table(col.1 = 1:5, col.2 = 11:15)
  for (iter.col in 1:2) {
    colname <- .PasteD("col", iter.col)
    expect_equal(test.dt[, .EvalText(colname, test.dt)],
                         10 * (iter.col - 1) + 1:5)
  }
  test.dt[,
          (.PasteD(colname, "plus1")) :=
              .EvalText(colname, test.dt) + 1]
  expect_equal(test.dt[, col.2.plus1], 12:16)
  # Default environment: parent.frame().
  x <- 1
  fn <- function() {
    x <- 2
    return(c(.EvalText("x"), .EvalText("x", parent.frame())))
  }
  expect_identical(c(2, 1), fn())
})

context(".CheckLength")

test_that("vector lengths are updated, or errors are thrown", {
  expect_error(.CheckLength(1:4, 1), "Argument of incorrect length")
  expect_identical(.CheckLength(1:4, 3, FALSE), 1:3)
  expect_identical(.CheckLength(2, 4), rep(2, 4))
  expect_identical(.CheckLength(1:4, 4), 1:4)
})

context(".MultiplyBySegment")

test_that("factors are mapped correctly to segments", {
  expect_identical(1, .MultiplyBySegment())
  expect_identical(as.numeric(kAllStates[, activity]) / 10,
                   .MultiplyBySegment(list(activity = (1:3) / 10)))
  expect_identical(as.numeric(kAllStates[, activity]) / 20,
                   .MultiplyBySegment(list(activity = (1:3) / 10), 0.5))
})

context(".ReadRepeatingVector")

test_that("the function finds the correct entry", {
  expect_identical(.ReadRepeatingVector(1:5, 3), 3L)
  expect_identical(.ReadRepeatingVector(1:5, 19), 4L)
  expect_error(.ReadRepeatingVector(1:5, 0),
               "idx not greater than or equal to 1")
})
