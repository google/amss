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

context("DefaultNatMigModule")

test_that("migration results in approximately the expected segmentation", {
  curr.dt <- .InitStateData()
  DefaultNatMigModule(
      curr.dt,
      population = 7e8L,
      market.rate.trend = 0.8,
      market.rate.seas = 1,
      sat.decay = 0.5)
  expect_equal(curr.dt[, sum(pop)], 7e8L)
  expect_equal(curr.dt[satiation == "satiated", sum(pop)], 0L)
  activity.prop <- curr.dt[market == "in.market" & satiation == "unsatiated",
                           sum(pop), by = "activity"][, V1 / sum(V1)]
  availability.prop <- curr.dt[, sum(pop), by = "availability"][, V1 / sum(V1)]
  expect_true(all(activity.prop %between% c(0.30, 0.37)))
  expect_true(all(availability.prop %between% c(0.30, 0.37)))
})

context(".InitPop")

test_that("the data is initialized correctly", {
  testpop.dt <- .InitStateData()
  testpop.dt[, pop.total := 100][, market.rate := 0.3]
  .InitPop(testpop.dt, pop.total = 100, market.rate = 0.3,
           prop.activity = c(0.5, 0.3, 0.2),
           prop.favorability = c(.15, .1, .2, .35, .2))
  # Population.
  expect_equal(100, testpop.dt[, sum(pop)])
  # Satiation is 0.
  expect_true(all(
      testpop.dt[.(c("out.market", "in.market"), "satiated"), pop == 0]))
})

context(".UpdateMarketingResponsiveStates")

test_that("migration happens in the correct sequence and proportions", {
  orig.data <- .InitStateData(time.index = 10)
  orig.data[, pop := rbinom(nrow(orig.data), 2000, 0.5)]
  copied.data <- data.table::copy(orig.data)

  # Use 0-1 matrices for testing since that removes randomness.
  activity.trans <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3,
                           byrow = TRUE)
  .UpdateMarketingResponsiveStates(
      copied.data,
      transition.matrices = list(activity = activity.trans))

  # Check there is no effect on distribution of other state types.
  expect_equal(orig.data[, sum(pop),
                         by = eval(key(orig.data)[-3])],
               copied.data[, sum(pop),
                           by = eval(key(orig.data)[-3])])
  # No effect on out of market or satiated individuals.
  expect_equal(orig.data["out.market", pop],
               copied.data["out.market", pop])
  expect_equal(orig.data[.("in.market", "satiated"), pop],
               copied.data[.("in.market", "satiated"), pop])
  # Check transition rates applied correctly.
  expect_equal(
      orig.data[.("in.market", "unsatiated"),
                pop %*% activity.trans,
                by = names(kBrandStates)][, V1],
      copied.data[.("in.market", "unsatiated"),
                  pop,
                  by = names(kBrandStates)][, pop])
})

context(".UpdateMarket")

test_that("migration in one dimension does not affect unrelated dimensions", {
  test.data <- .InitStateData(time.index = 10)
  test.data[, pop := rbinom(nrow(test.data), 2000, 0.5)]
  GetRate <- function(dt) {
    return(dt[market == "in.market", sum(pop)] / dt[, sum(pop)])
  }
  curr.rate <- GetRate(test.data)
  # Try increasing.
  copy.data <- data.table::copy(test.data)
  .UpdateMarket(copy.data, 1)
  expect_equal(1, GetRate(copy.data))
  expect_equal(test.data[, sum(pop),
                         by = c("satiation", "favorability",
                                "loyalty", "availability")],
               copy.data[, sum(pop),
                         by = c("satiation", "favorability",
                                "loyalty", "availability")])
  # Try decreasing.
  copy.data <- data.table::copy(test.data)
  .UpdateMarket(copy.data, 0)
  expect_equal(0, GetRate(copy.data))
  expect_equal(test.data[, sum(pop),
                         by = c("satiation", "favorability",
                                "loyalty", "availability")],
               copy.data[, sum(pop),
                         by = c("satiation", "favorability",
                                "loyalty", "availability")])
})
