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

context("SimulateCorrelated")

test_that("constant v results in pure noise", {
  set.seed(100)
  # Even without seed set, keep test flakiness to a minimum with large n,
  # small alpha.
  n <- 5000
  alpha <- 0.0001
  # correlate with constant should be a regular normal distribution
  # with the specified mean and variance
  expect_true(ks.test(
      SimulateCorrelated(v = rep(0, n), cor.vx= 0.2,
                         mu.x = 77, sigma.x = 2.3),
      function(q) pnorm(q, 77, 2.3))$p.value >= alpha)
})

context("SimulateDummy")

test_that("dummy variables repeat, scale, and signal errors correctly", {
  expect_error(SimulateDummy(n = -4))
  expect_error(SimulateDummy(n = 10, pos.idx = NULL, period = -2))
  expect_error(SimulateDummy(n = 4, pos.idx = 1, period = 2.3))
  expect_equal(SimulateDummy(n = 4), rep(0, 4))
  expect_equal(SimulateDummy(n = 10, pos.idx = 1:2, period = 5),
               rep(c(1, 1, 0, 0, 0), 2))
  expect_equal(SimulateDummy(n = 2, pos.idx = 1, amplitude = 2), c(2, 0))
})

context("SimulateSinusoidal")
test_that("sinusoidals translate, scale, and signal errors correctly", {
  expect_error(SimulateSinusoidal(n = -4, period = 0))
  expect_error(SimulateSinusoidal(n = 100, period = 0:10))
  expect_error(SimulateSinusoidal(n = 100, period = -1, 1))
  expect_equal(SimulateSinusoidal(n = 100, period = 1), rep(1, 100))
  expect_error(SimulateSinusoidal(n = 100, period = 1, amplitude = -1))
  expect_equal(SimulateSinusoidal(n = 100, period = 2, max.loc = 4,
                                  vert.translation = 1, amplitude = 2),
               rep(c(-1, 3), 50))
  expect_equal(mean(SimulateSinusoidal(n = 100, period = 2, max.loc = 4,
                                       vert.translation = 1, amplitude = 2,
                                       scale.x = TRUE)), 0)
  expect_equal(sd(SimulateSinusoidal(n = 100, period = 2, max.loc = 4,
                                     vert.translation = 1, amplitude= 2,
                                     scale.x = TRUE)), 1)
})

context("SimulateAR1")

test_that("errors are signalled as appropriate", {
  expect_error(SimulateAR1(n = -4))
  expect_error(SimulateAR1(n = 100, stable.mu = 1, stable.sd = 3, autocor = -1))
  expect_error(SimulateAR1(n = 100, stable.mu = 0, stable.sd = -1))
})

test_that("output vectors follow the specified distribution", {
  expect_equal(SimulateAR1(n = 100, stable.mu = 0,
                           stable.sd = 0, autocor = 0.2),
               rep(0, 100))
  NearEnoughAR1 <- function(ts, mu, sd, ac, c.level = 0.9999) {
    y <- ts[2:length(ts)]
    x <- ts[1:(length(ts) - 1)]
    lm.obj <- lm(y ~ x)
    s2 <- crossprod(lm.obj$resid) / (length(y) - 2)
    est.bds <- rbind(confint(lm.obj, level = c.level),
                     s2 = c(s2 / (length(y) - 2) *
                            qchisq((1 - c.level) / 2, length(y) - 2),
                            s2 / (length(y) - 2) *
                            qchisq((1 - c.level) / 2, length(y) - 2,
                            lower.tail = FALSE)))
    row.names(est.bds) <- c("c", "phi", "sigma2")
    theta <- c((1 - ac) * mu, ac, (1 - ac^2) * sd^2)
    return(theta >= est.bds[, 1] & theta <= est.bds[, 2])
  }
  expect_true(all(NearEnoughAR1(SimulateAR1(5000, 8, 8, 0.8),
                                8, 8, 0.8)))
})
