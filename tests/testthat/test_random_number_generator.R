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

context("RBinom")

test_that("Binomial samples are generated for a large number of trials.", {
  M <- .Machine$integer.max
  expect_true(!is.na(RBinom(1, 100 * M, 0.5)))
  expect_identical(class(RBinom(1, 2 * M, 0.1)), "numeric")
  expect_identical(class(RBinom(1, 100 * M, 0.1)), "numeric")
  # Check that the sample p.hat is close to p for large values of size.
  expect_true(all(abs(RBinom(2, 2 * M, c(0.01, 0.99)) / (2 * M) /
                      c(0.01, 0.99) - 1) < 0.01))
  expect_true(all(abs(RBinom(2, 1e5 * M, c(0.1, 0.9)) /
                      (1e5 * M) / c(0.1, 0.9) - 1) < 0.01))
})

context("RHyper")

test_that("Hypergeometric samples are generated for a large population size.", {
  # Remove test flakiness.
  set.seed(0)
  M <- .Machine$integer.max
  # Check approximation accuracy through probability of sample quantile under
  # exact distribution
  # Check a binomial approximation.
  x <- RHyper(1e5, 1e5, M, 1000)
  expect_true(binom.test(sum(x == 0), 1e5, phyper(0, 1e5, M, 1000))$p.value >
              0.01)
  # Check a normal approximation.
  k <- round(600 * M * 4 / 5)
  x <- RHyper(1e5, 100 * M, 500 * M, k)
  q <- quantile(x, 0.1)
  expect_true(binom.test(sum(x <= q), 1e5,
                         phyper(q, 100 * M, 500 * M, k))$p.value > 0.01)
})

context("RMultinom")

test_that("Multinomial samples are generated for a large number of trials.", {
  x <- RMultinom(10, .Machine$integer.max * 2, rep(1 / 5, 5))
  expect_identical(class(x[1]), "numeric")
  x <- RMultinom(10, .Machine$integer.max * 10, 1:5)
  expect_identical(class(x[1]), "numeric")
  # Column sums should equal the size parameter.
  expect_equal(apply(x, 2, sum), rep(.Machine$integer.max * 10, 10))
  # Row sums should be approximately proportional to the probability vector.
  x.prop <- apply(x, 1, sum)
  x.prop <- x.prop / sum(x.prop)
  expect_true(all(abs(x.prop - (1:5) / sum(1:5)) < 0.01))
})

context("RNBinom")

test_that("NBinom samples are generated for a large number of trials.", {
  size <- c(1e-4, 1e-2, 1, 1e2, 1e4) * .Machine$integer.max * 2
  expect_true(!any(is.na(RNBinom(5, size, 0.5))))
  expect_identical(class(RNBinom(5, size, 0.5)), "numeric")
  expect_identical(class(RNBinom(2, size, 0.5)), "numeric")
  # Check the 10% quantile of the approximate samples.
  x <- RNBinom(1e5, size[5], 0.5)
  q <- quantile(x, 0.10)
  expect_true(binom.test(sum(x <= q), 1e5, pnbinom(q, size[5], 0.5))$p.value >
              0.1)
})

context("RPois")

test_that("Poisson samples are generated for a large mean.", {
  lambda <- c(1e-4, 1e-2, 1, 1e2, 1e4) * .Machine$integer.max
  expect_true(!any(is.na(RPois(5, lambda))))
  expect_identical(class(RPois(5, lambda)), "numeric")
  expect_identical(class(RPois(2, lambda)), "numeric")
  # Check the 10% quantile of the approximate samples.
  x <- RPois(1e5, lambda[5])
  q <- quantile(x, 0.10)
  expect_true(binom.test(sum(x <= q), 1e5, ppois(q, lambda[5]))$p.value >
              0.1)
})
