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

context(".MigrateMarginal")

test_that("one-dimensional migration is handled correctly", {
  dt <- .InitStateData()
  dt[, pop := rbinom(nrow(dt), 2000, 0.5)]
  aff.pop <- sapply(dt[, pop], function(n) rbinom(1, n, 0.5))
  copy.dt <- data.table::copy(dt)

  # Dimension 1: market state.
  .MigrateMarginal(copy.dt, aff.pop, "market", matrix(c(0, 1), 2, 2, TRUE))
  expect_equal(copy.dt[market == "out.market", pop],
               dt[market == "out.market", pop] -
                  aff.pop[dt[, market == "out.market"]])
  expect_equal(dt[, sum(pop)], copy.dt[, sum(pop)])

  # Dimension 5: loyalty state.
  copy.dt <- data.table::copy(dt)
  .MigrateMarginal(copy.dt, aff.pop, "loyalty", matrix(c(0, 1, 0), 3, 3, TRUE))
  dt[, aff.pop := aff.pop]
  expect_equal(
      copy.dt[favorability != "favorable", sum(pop), by = "loyalty"][, V1],
      c(dt[favorability != "favorable", sum(aff.pop)], 0) +
      dt[favorability != "favorable", sum(pop - aff.pop), by = "loyalty"][, V1])
  expect_equal(copy.dt[favorability == "favorable" & loyalty == "loyal",
                       pop.in],
               copy.dt[favorability == "favorable", sum(pop.out),
                       by = eval(key(copy.dt)[-5])][, V1])
  expect_equal(dt[, sum(pop)], copy.dt[, sum(pop)])
})
