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

context("DefaultTraditionalMediaModule")

test_that("the specified amount of spend is generated", {
  traditional.spend <-
      sapply(test.data$data.full,
             function(x) x[, sum(traditional.spend)])
  expect_equal(sum(traditional.spend[1:10]), 0)
  media.params <- test.args$media.params[[1]]
  for (idx in unique(media.params$budget.index)) {
    curr.timepoints <- media.params$budget.index == idx
    expect_equal(media.params$budget[idx],
                 sum(traditional.spend[curr.timepoints]))
    expect_equal(
        media.params$flighting[curr.timepoints] /
            sum(media.params$flighting[curr.timepoints]) *
            media.params$budget[idx],
        traditional.spend[curr.timepoints])
  }
})

context("DefaultSearchMediaModule")

test_that("the function completes successfully", {
  dt <- InitStateData(time.index = 13)
  InitPop(data.dt = dt, pop.total = test.args$nat.mig.params$population,
          market.rate = 0.7,
          prop.activity = c(0.5, 0.3, 0.2),
          prop.favorability = c(0.15, 0.1, 0.2, 0.35, 0.2))
  do.call(DefaultSearchMediaModule,
          c(list(dt), test.args$media.params[[2]]))
  expect_true(setequal(names(dt),
              c(names(kAllStates),
                "time.index",
                "pop", "pop.out", "pop.in",
                "budget.index", "budget",
                "audience", "ctr",
                "clicks", "imps", "matching.query.volume", "query.volume",
                "spend")))
})
