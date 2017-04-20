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

#' Simulate correlated vectors.
#'
#' Simulates a new vector x with a specified mean, standard deviation, and
#' correlation with some other vector \code{v} by adding white noise and
#' scaling.
#'
#' @param v vector to which the new data will be correlated.
#' @param cor.vx numeric specifying correlation between v and the new vector x.
#' @param mu.x numeric, mean of the new vector x.
#' @param sigma.x numeric, standard deviation of the new vector x.
#' @return a vector x with the specified mean, standard deviation, and
#'   correlation.
#' @family Simulate time series
#' @export

SimulateCorrelated <- function(v, cor.vx = 1, mu.x = 0, sigma.x = 1) {

  # Check inputs.
  assertthat::assert_that(is.numeric(v))
  assertthat::assert_that(is.numeric(cor.vx), length(cor.vx) == 1,
                          abs(cor.vx) <= 1)
  assertthat::assert_that(is.numeric(mu.x), length(mu.x) == 1)
  assertthat::assert_that(is.numeric(sigma.x), length(sigma.x) == 1,
                          sigma.x >= 0)

  # If v is constant, then x is just noise.
  if (min(v) == max(v)) {
    x <- rnorm(length(v))
  } else {  # Else, add the appropriate level of noise.
    x <- scale(v) * cor.vx + rnorm(n = length(v), sd = sqrt(1 - cor.vx ^ 2))
  }
  return(mu.x + sigma.x * x)
}

#' Simulate dummy (0-1) variables.
#'
#' Create dummy (0-1) variables that repeat a requested pattern of 0's and 1's,
#' with the option to scale.
#'
#' @param n integer number of time points
#' @param pos.idx vector of indices where simulated vector should take positive
#'   values (as opposed to zero)
#' @param period integer controlling periodicity.
#' @param amplitude numeric value > 0 specifying the value of all postive
#'   entries in the return vector.
#' @return specified vector
#' @family Simulate time series
#' @export

SimulateDummy <- function(n, pos.idx = NULL, period = n, amplitude = 1) {

  # Check inputs.
  assertthat::assert_that(is.numeric(n), length(n) == 1,
                          as.integer(n) == n, n >= 0)
  assertthat::assert_that(is.null(pos.idx) || is.numeric(pos.idx))
  assertthat::assert_that(is.numeric(period), length(period) == 1,
                          as.integer(period) == period, period >= 1)
  assertthat::assert_that(is.numeric(amplitude), length(amplitude) == 1,
                          amplitude >= 0)

  # Return the specified vector.
  return(amplitude * as.numeric(((1:n) %% period) %in% pos.idx))
}

#' Generate sinusoidal time series.
#'
#' Function that outputs specified sinusoidal waves.
#'
#' @param n the length of the simulated vector.
#' @param period the length of one full sinusoidal period.
#' @param max.loc the index of the maximum of the sinusoidal curve.
#' @param vert.translation a numeric for the vertical displacement of the
#'   sinusoidal curve from 0.
#' @param amplitude numeric for the amplitude of the sinusoidal curve. Must be
#'   nonnegative.
#' @param scale.x boolean. If TRUE, scale the sinusoidal curve to have mean 0
#'   and standard deviation 1 before returning.
#'
#' @return specified sinusoidal curve as a vector
#' @family Simulate time series
#' @export

SimulateSinusoidal <- function(n, period, max.loc = 1,
                               vert.translation = 0, amplitude = 1,
                               scale.x = FALSE) {

  # Check inputs.
  assertthat::assert_that(is.numeric(n), length(n) == 1,
                          as.integer(n) == n, n >= 0)
  assertthat::assert_that(is.numeric(period), length(period) == 1,
                          period > 0)
  assertthat::assert_that(is.numeric(max.loc), length(max.loc) == 1)
  assertthat::assert_that(is.numeric(amplitude), length(amplitude) == 1,
                          amplitude >= 0)
  assertthat::assert_that(assertthat::is.flag(scale.x))

  # Calculate the values of the sinusoidal curve.
  x <- cos(((1:n) - max.loc) / period * 2 * pi) * amplitude + vert.translation

  # Scale x if flagged, and then return.
  if (scale.x) {
    return(scale(x))
  } else {
    return(x)
  }
}

#' Simulate AR1 time series
#'
#' Function that outputs simulated AR1 time series with specified means,
#' variances, and autocorrelations
#'
#' @param n integer number of time points
#' @param stable.mu means of the stable distribution for each variable
#' @param stable.sd standard deviations of the stable distributions
#' @param autocor autocorrelations for each time series
#' @return vector realization of specified AR1 times series
#' @family Simulate time series
#' @export

SimulateAR1 <- function(n, stable.mu = 0, stable.sd = 1, autocor = 0) {

  # Check inputs.
  assertthat::assert_that(is.numeric(n), length(n) == 1,
                          as.integer(n) == n, n >= 0)
  assertthat::assert_that(is.numeric(stable.mu), length(stable.mu) == 1)
  assertthat::assert_that(is.numeric(stable.sd), length(stable.sd) == 1,
                          stable.sd >= 0)
  assertthat::assert_that(is.numeric(autocor), length(autocor) == 1,
                          abs(autocor) < 1)

  # Translate the stable distribution into AR1 parameters, then generate time
  # series.
  if (stable.sd == 0) {
    return(rep(stable.mu, n))
  }
  if (autocor == 0) {
    return(rnorm(n, stable.mu, stable.sd))
  }
  c <- (1 - autocor) * stable.mu
  sigma <- sqrt(1 - autocor ^ 2) * stable.sd
  return(as.vector(arima.sim(list(ar = autocor), n, sd = sigma, mean = c)))
}
