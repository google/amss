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

context("CalculateROAS")

test_that("Attribution results are reasonable", {
  set.seed(100)
  mroas <- CalculateROAS(test.data,
                         media.names = "traditional",
                         budget.periods = 3,
                         t.start = 21, t.end = 30,
                         budget.proportion = 0.99)
  roas <- CalculateROAS(test.data,
                        media.names = "traditional",
                        budget.periods = 3,
                        t.start = 21, t.end = 30,
                        budget.proportion = 0)
  expect_true(mroas > 0 & mroas < 0.01)
  expect_true(roas > 0.26 & roas < 0.27)
})

context(".GenerateDataUnderNewBudget")

test_that("data regeneration starts and ends at the correct time", {
  new.data.1 <- GenerateDataUnderNewBudget(test.data)
  expect_identical(names(new.data.1), c(names(test.data$data), "rep.index"))
  # Start after first time period.
  new.data.2 <- GenerateDataUnderNewBudget(
      test.data, t.start = 11, t.end = 20)
  id.idx <- test.args$media.params[[1]]$budget.index == 1
  expect_equal(new.data.2[id.idx, brand.sales],
               test.data$data[id.idx, brand.sales])
  # Multiple reps, and response metric instead of entire dataset.
  set.seed(100)
  new.data.4 <- GenerateDataUnderNewBudget(
      test.data, t.start = 31, reps = 2)
  set.seed(100)
  new.data.5 <- GenerateDataUnderNewBudget(
      test.data, t.start = 31,
      reps = 2, response.metric = "brand.sales")
  expect_identical(nrow(new.data.4), 2L * nrow(new.data.1))
  expect_equal(
      new.data.4[, mean(brand.sales), by = "time.index"][, V1],
      new.data.5)
})
