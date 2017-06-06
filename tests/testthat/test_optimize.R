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

context("GetInterior")

test_that("Interior points are correctly identified.", {
  # Finite and infinite bounds both result in interior points being returned.
  x1 <- GetInterior(1, 2, 1, 2)
  expect_true(x1 > 1 && x1 < 2)
  x2 <- GetInterior(rep(-Inf, 5), rep(Inf, 5), -Inf, Inf)
  expect_true(is.numeric(x2) && length(x2) == 5)
  x3 <- GetInterior(c(-Inf, 1, 4), c(-5, 100, Inf), -Inf, Inf)
  expect_true(x3[1] < -5 && x3[2] > 1 && x3[2] < 100 && x3[3] > 4)

  # settings with no interior return NULL
  expect_identical(NULL, GetInterior(1, -1, 0, 0))
  expect_identical(NULL, GetInterior(1:2, 3:4, -9, 0))
  expect_identical(NULL, GetInterior(1:2, 3:4, 5, 5))
})

context("ReduceDimension")

test_that("Optimization complexity successfully reduced when appropriate.", {
  # Number of dimensions must be specified correctly.
  expect_error(ReduceDimension(3, 1:5, 1:5, 15, 15))

  # Reduce dimension. This case includes equality constraints in both the
  # individual bounds on single dimensions and the bounds on the vector sum.
  lower.bound <- 1:7
  upper.bound <- c(1:5, 7, 9)
  object <- ReduceDimension(7, 1:7, c(1:5, 7, 9), 29.5, 29.5)
  expect_identical(length(object$lower.bound), 1L)
  v <- (max(object$lower.bound, object$sum.lower.bound) +
        min(object$upper.bound, object$sum.upper.bound)) / 2
  x <- object$decoder(v)
  expect_true(all(x >= 1:7) && all(x <= c(1:5, 7, 9)) && sum(x) == 29.5)
})
