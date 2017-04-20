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

context("InitStateData")

test_that("the population segmentation states are generated correctly", {
  # Test package constants have correct value along the way.
  init.data <- .InitStateData()
  expect_identical(nrow(init.data), nrow(kAllStates))
  expect_identical(nrow(kAllStates), nrow(kBrandStates) * nrow(kCategoryStates))
  expect_identical(nrow(kCategoryStates), 6L)
  expect_identical(nrow(kBrandStates), 33L)
  expect_identical(names(init.data),
              c("market", "satiation", "activity",
                "favorability", "loyalty", "availability",
                "time.index", "geo.index", "pop"))
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
  expect_identical(init.data[, pop], rep(0L, nrow(init.data)))
})
