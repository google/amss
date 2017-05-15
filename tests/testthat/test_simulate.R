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

context("SimulateAMSS")

test_that("variables have reasonable values", {
  # Test that simulated data achieves specified population.
  expect_true(all(sapply(test.data$data.full,
                         function(x) x[, sum(pop) == 1000])))
  expect_true(cor(test.data$data[, revenue],
              test.args$nat.mig.params$market.rate.seas) > 0.5)
})

context("SimulateData")

test_that("data generates correctly, starting from an arbitrary time", {
  # Parameter values.
  for (iter.arg in names(test.args)) {
    EvalText(paste0(iter.arg, " <- test.args$", iter.arg))
  }

  # Data generation: Complete data generated for TestSimulateAMSS().
  # Here, first try simulating less data than the original time.n
  data.first10 <- SimulateData(
      starting.dts = list(),
      time.n = 10,
      nat.mig.module = DefaultNatMigModule,
      nat.mig.params = nat.mig.params,
      media.names = media.names[1],
      media.modules = list(DefaultTraditionalMediaModule),
      media.params = media.params[1],
      sales.module = DefaultSalesModule,
      sales.params = sales.params,
      ping = 10)
  # Then, try simulating additional data following up.
  data.next10 <- SimulateData(
      starting.dts = data.first10,
      time.n = 20,
      nat.mig.module = DefaultNatMigModule,
      nat.mig.params = nat.mig.params,
      media.names = media.names[1],
      media.modules = list(DefaultTraditionalMediaModule),
      media.params = media.params[1],
      sales.module = DefaultSalesModule,
      sales.params = sales.params,
      ping = 10)

  # First 10 elements should be identical.
  expect_equal(data.first10[1:10], data.next10[1:10])

  # The two data tables are indeed different (copy() worked correctly),
  # i.e., they map to different memory locations.
  data.first10[[1]][, profit := 0]
  expect_true(all(data.first10[[1]][, profit == 0]))
  expect_true(!isTRUE(data.next10[[1]][, profit == 0]))

  # Check that the latter data set generated numbers appropriately.
  expect_equal(media.params[[1]]$flighting[1:10] *
               media.params[[1]]$budget[1],
               SurfaceData(data.first10)[, traditional.spend])
  expect_equal(media.params[[1]]$flighting[1:20] *
               rep(media.params[[1]]$budget[1:2], each = 10) /
               rep(c(sum(media.params[[1]]$flighting[1:10]),
                     sum(media.params[[1]]$flighting[11:20])),
                   each = 10),
               SurfaceData(data.next10)[, traditional.spend])
})

context("SurfaceData")

test_that("observed data is aggregated correctly", {
  obs.data <- SurfaceData(full.data = test.data$data.full)

  # Check values in some 'sum' rows.
  # Profit = revenue - spend.
  expect_equal(rep(0, nrow(obs.data)),
               obs.data[, profit - (revenue - total.spend)])

  # Spend in media equals budget.
  expect_equal(test.args$media.params[[1]]$budget,
               obs.data[, sum(traditional.spend),
                        by = traditional.budget.index][, V1])

  # Exceptions for bad arguments:
  # Aggregate one column both ways.
  expect_error(SurfaceData(test.data$data.full,
                           "total.spend", "total.spend"),
               "The same variable cannot be aggregated in multiple ways.")

  # Having 0 columns aggregated one or both ways is fine.
  expect_equal(
      as.matrix(obs.data[, .(time.index)]),
      as.matrix(SurfaceData(test.data$data.full,
                            character(), character())))
  dt1 <- SurfaceData(full.data = test.data$data.full,
                     names.const = character())
  dt2 <- SurfaceData(full.data = test.data$data.full,
                     names.sum = character())
  expect_equal(as.matrix(obs.data),
               as.matrix(dt1[dt2]))
})

context("InitStateData")

test_that("the population segmentation states are generated correctly", {
  # Test package constants have correct value along the way.
  init.data <- InitStateData()
  expect_identical(nrow(init.data), nrow(kAllStates))
  expect_identical(nrow(kAllStates), nrow(kBrandStates) * nrow(kCategoryStates))
  expect_identical(nrow(kCategoryStates), 6L)
  expect_identical(nrow(kBrandStates), 33L)
  expect_identical(names(init.data),
              c("market", "satiation", "activity",
                "favorability", "loyalty", "availability",
                "time.index", "pop"))
  expect_identical(key(init.data),
              c("market", "satiation", "activity",
                "favorability", "loyalty", "availability"))
  expect_identical(levels(init.data[, market]),
              c("out.market", "in.market"))
  expect_identical(levels(init.data[, satiation]),
              c("satiated", "unsatiated"))
  expect_identical(levels(init.data[, activity]),
              c("inactive", "exploration", "purchase"))
  expect_identical(levels(init.data[, favorability]),
              c("unaware", "negative", "neutral",
                "somewhat favorable", "favorable"))
  expect_identical(levels(init.data[, loyalty]),
              c("switcher", "loyal", "competitor-loyal"))
  expect_identical(levels(init.data[, availability]),
              c("low", "average", "high"))
  expect_identical(init.data[, pop], rep(0, nrow(init.data)))
})
