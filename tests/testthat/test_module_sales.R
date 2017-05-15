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

context("DefaultSalesModule")

test_that("advertiser and competitor sales are calculated correctly", {
  dt <- InitStateData(time.index = 13)
  dt[, pop := RBinom(nrow(kAllStates), 2000, 0.5)]
  init.pop <- dt[, pop]
  dt[, total.spend := pop * 0.5]
  DefaultSalesModule(
      data.dt = dt, price = 20,
      advertiser.demand.intercept = list(
          favorability = c(0, 0, 0, 1, 1)))
  expect_identical(dt[, brand.sales + competitor.sales],
                   init.pop * kAllStates[, activity == "purchase"])
  # By default, the competitor replacement parameter is c(0.5, 0, 1)
  # and competitor strength is 1. Since the non-competitive advertiser demand
  # is 1. Then:
  # Competitor-loyal consumers should not purchase the advertiser's brand.
  expect_true(all(
      dt[activity == "purchase" & loyalty == "competitor-loyal",
         brand.sales == 0]))
  # Loyal consumers should not purchase any competitor brands.
  expect_true(all(
      dt[activity == "purchase" & loyalty == "loyal",
         competitor.sales == 0]))
})
