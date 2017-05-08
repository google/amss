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

#' Simulate random number from the binomial distribution.
#'
#' Generate random integers from the binomial distribution when possible within
#' integer overflow constraints. Otherwise, approximate with the normal
#' distribution.
#'
#' @param n number of observations. If \code{length(n) > 1}, the length is
#'   taken to be the number required.
#' @param size number of trials (zero or more). When \code{size} is a vector,
#'   the number of trials for each observation. Conflicts between the number of
#'   observations \code{n} and the length of the size vector are resolved by
#'   truncating or repeating it to length \code{n}. This matches the behavior of
#'   the original \code{rbinom()}.
#' @param prob probability of success on each trial. When \code{prob} is a
#'   vector, each entry refers to the probability of success for trials
#'   associated with the corresponding observation. Conflicts between the
#'   number of observations \code{n} and the length of the probability vector
#'   are resolved by truncating or repeating it to length \code{n}. This matches
#'   the behavior of the original \code{rbinom()}.
#' @return Random numbers from the binomial distribution, or its normal
#'   approximation. The function will return a numeric value, rather than an
#'   integer.
#' @keywords internal

RBinom <- function(n, size, prob) {

  # If size is not too large, use regular rbinom.
  if (all(size <= .Machine$integer.max)) {
    return(as.numeric(rbinom(n, size, prob)))
  }

  # Check and interpret arguments.
  if (length(n) > 1) {
    n <- length(n)
  }
  size <- rep(size, length = n)
  prob <- rep(prob, length = n)
  assertthat::assert_that(all(prob >= 0 & prob <= 1))

  # Generate either x or size - x, whichever is smaller.
  prob.min <- pmin(prob, 1 - prob)
  # Calculate probability of integer overflow.
  p.overflow <- pbinom(.Machine$integer.max, size, prob.min, lower.tail = FALSE)
  x <- suppressWarnings(as.numeric(rbinom(n, size, prob.min)))

  # Approximate with the normal distrbution if probability of overflow > 0.
  # Do this even if x is not NA, to avoid double-dipping.
  # Checking probability of integer overflow rather than size covers the edge
  # case where prob is very small and size is very large.
  p.overflow.idx <- p.overflow > 0
  if (any(p.overflow.idx)) {
    approx.x <- rnorm(sum(p.overflow.idx),
                      size[p.overflow.idx] * prob.min[p.overflow.idx],
                      sqrt(size[p.overflow.idx] * prob.min[p.overflow.idx] *
                           (1 - prob.min[p.overflow.idx])))
    approx.x <- round(pmax(0, pmin(size[p.overflow.idx], approx.x)))
    x[p.overflow.idx] <- approx.x
  }

  # Convert size - x to x where necessary.
  flip.idx <- prob > 0.5
  x[flip.idx] <- size[flip.idx] - x[flip.idx]
  return(x)
}

#' Simulate random number from the hypergeometric distribution.
#'
#' Generate random integers from the hypergeometric distribution when possible
#' within integer overflow constraints. Otherwise, approximate with the normal
#' distribution.
#'
#' @details
#' The hypergeometric distribution is used for sampling the number of white
#' balls drawn when a fixed number of balls is drawn without replacement from
#' an urn which contains both black and white balls.
#'
#' @param nn number of observations. If \code{length(nn) > 1}, the length is
#'   taken to be the number required.
#' @param m the number of white balls in the urn.
#' @param n the number of black balls in the urn.
#' @param k the number of balls drawn from the urn.
#' @return Random numbers from the binomial distribution, or its normal
#'   approximation. The function will a numeric value, rather than an integer.
#' @keywords internal

RHyper <- function(nn, m, n, k) {

  # Use rhyper() where possible.
  # rhyper() works as long as m + n is within the bounds for integers.
  # It also works for the edge case of 0 observations.
  x <- suppressWarnings(as.numeric(rhyper(nn, m, n, k)))
  if (!any(is.na(x))) {
    return(x)
  }

  # Check and interpret arguments.
  if (length(nn) > 1) {
    nn <- length(nn)
  }
  assertthat::assert_that(is.numeric(nn), nn > 0)
  assertthat::assert_that(is.numeric(m), all(m >=0))
  m <- rep(m, length = nn)
  assertthat::assert_that(is.numeric(n), all(n >= 0))
  n <- rep(n, length = nn)
  assertthat::assert_that(is.numeric(k), all(k >= 0))
  k <- rep(k, length = nn)
  m.plus.n <- as.numeric(m) + as.numeric(n)
  assertthat::assert_that(all(k <= m.plus.n))


  # Use the binomial approximation for k close to 0 or the total number of
  # balls.
  p <- m / m.plus.n
  size.k <- k / m.plus.n
  # When k is very small compared to (m + n), use a binomial approximation.
  binom.idx <- is.na(x) & size.k < 1e-3
  x[binom.idx] <- RBinom(sum(binom.idx), k[binom.idx], p[binom.idx])
  # When k is very close to (m + n), use a binomial approximation on the
  # number of balls left in the urn.
  rev.binom.idx <- is.na(x) & size.k > (1 - 1e-3)
  x[rev.binom.idx] <- m[rev.binom.idx] - RBinom(
      sum(rev.binom.idx),
      m.plus.n[rev.binom.idx] - k[rev.binom.idx],
      p[rev.binom.idx])

  # Else, use a normal approximation.
  normal.idx <- is.na(x)
  if (any(normal.idx)) {
    x[normal.idx] <- round(rnorm(
        sum(normal.idx),
        k[normal.idx] * p[normal.idx],
        sqrt(k[normal.idx] * p[normal.idx] * (1 - p[normal.idx]) *
             ((m.plus.n[normal.idx] - k[normal.idx]) /
              (m.plus.n[normal.idx] - 1)))))
  }

  # Force x within range of possible values.
  lower.bound <- pmax(0, k - n)
  upper.bound <- pmin(k, m)
  x <- pmin(upper.bound, pmax(lower.bound, x))

  return(x)
}

#' Simulate random number from the multinomial distribution.
#'
#' Generate random integers from the binomial distribution when possible within
#' integer overflow constraints. Otherwise, approximate with the normal
#' distribution.
#'
#' @param n the number of random vectors to draw
#' @param size integer, say \eqn{N}, specifying the total number of objects
#'   that are put into \eqn{K} boxes in the typical multinomial experiment.
#' @param prob numeric non-negative vector of length \eqn{K}, specifying the
#'   probability for the \eqn{K} classes; is internally normalized to sum 1.
#'   Infinite and missing values are not allowed.
#' @return Matrix of random numbers from the multinomial distribution, or its
#'   normal approximation. The function will return a numeric value, rather than
#'   an integer.
#' @keywords internal

RMultinom <- function(n, size, prob) {

  # If size is not too large, use regular rmultinom.
  if (all(size <= .Machine$integer.max)) {
    return(as.numeric(rmultinom(n, size, prob)))
  }

  # Check and interpret arguments.
  # Mimic rmultinom() in handling vector n, size by taking first entry.
  assertthat::assert_that(is.numeric(n))
  n <- n[1]
  assertthat::assert_that(is.numeric(size))
  size <- size[1]
  # Normalize prob.
  assertthat::assert_that(is.numeric(prob), !any(is.na(prob)))
  assertthat::assert_that(all(prob >= 0), any(prob > 0), all(is.finite(prob)))
  prob <- prob / sum(prob)

  # Generate multinomial numbers by iteratively calling the integer overflow
  # safe binomial random number generator.
  x <- matrix(0, length(prob), n)
  current.size <- size
  current.prob <- prob
  for (iter.box in 1:length(prob)) {
    x[iter.box, ] <- RBinom(n, current.size, current.prob[1])
    if (current.prob[1] == 1) {
      break
    }
    current.size <- current.size - x[iter.box, ]
    current.prob <- current.prob[-1] / sum(current.prob[-1])
  }

  return(x)
}

#' Simulate random number from the negative binomial distribution.
#'
#' Generate random integers from the negative binomial distribution when
#' possible within integer overflow constraints. Otherwise, approximate with the
#' normal distribution.
#'
#' @param n number of observations. If \code{length(n) > 1}, the length is
#'   taken to be the number required.
#' @param size number of trials (zero or more). When \code{size} is a vector,
#'   the number of trials for each observation.
#' @param prob probability of success on each trial. When \code{prob} is a
#'   vector, each entry refers to the probability of success for trials
#'   associated with the corresponding observation.
#' @param mu alternative parametrization via mean: see 'Details' in
#'   documentation for \code{rnbinom()}.
#' @return Random numbers from the negative binomial distribution, or its normal
#'   approximation. The function will return a numeric value, rather than an
#'   integer.
#' @keywords internal

RNBinom <- function(n, size, prob, mu) {

  # If size is not too large, use regular rbinom().
  if (all(size <= .Machine$integer.max)) {
    return(as.numeric(rnbinom(n, size, prob, mu)))
  }

  # Check and interpret arguments.
  if (length(n) > 1) {
    n <- length(n)
  }
  size <- rep(size, length = n)
  if (!missing(prob)) {
    prob <- rep(prob, length = n)
  }
  # Check to see if mu specified, convert to prob if so.
  if (!missing(mu)) {
    mu <- rep(mu, length = n)
    if (!missing(prob)) {
      stop("'prob' and 'mu' both specified")
    }
    prob <- size / (size + mu)
  }
  assertthat::assert_that(all(prob >= 0 & prob <= 1))

  # Calculate probability of integer overflow.
  p.overflow <- pnbinom(.Machine$integer.max, size, prob, lower.tail = FALSE)
  # Use rbinom() if safe from overflow.
  x <- suppressWarnings(as.numeric(rnbinom(n, size, prob)))
  if (all(p.overflow == 0)) {
    return(x)
  }

  # Else, approximate with the normal distrbution.
  approx.x <- rnorm(n, size * (1 - prob) / prob,
                    sqrt(size * (1 - prob)) / prob)
  approx.x <- round(pmax(0, pmin(size, approx.x)))
  x[p.overflow > 0] <- approx.x[p.overflow > 0]
  return(x)
}

#' Simulate random number from the Poisson distribution.
#'
#' Generate random integers from the Poisson distribution if possible, given
#' integer overflow constraints. Otherwise, approximate the Poisson with the
#' normal distribution.
#'
#' @param n number of random values to return. If \code{length(n) > 1}, the
#'   length is taken to be the number required.
#' @param lambda vector of (non-negative) means.
#' @return RPois returns random numbers from the Poisson distribution, or its
#'   normal approximation. The function will return a numeric value, rather than
#'   an integer.
#' @keywords internal

RPois <- function(n, lambda) {

  # Check and interpret arguments.
  if (length(n) > 1) {
    n <- length(n)
  }
  lambda <- rep(lambda, length = n)
  assertthat::assert_that(all(lambda >= 0))

  # Check whether there is a chance of integer overflow.
  # Let M be the maximum integer value. From the Chernoff bound, when
  # lambda = M / exp(1), P(X >= M) <= exp(-M / exp(1)) \approx 0.
  overflow.idx <- lambda > .Machine$integer.max / exp(1)

  # Use regular rpois().
  x <- as.numeric(suppressWarnings(rpois(n, lambda)))

  # Calculate the normal approximation when there is a nonzero chance of
  # integer overflow
  x[overflow.idx] <- pmax(
      0,
      round(rnorm(sum(overflow.idx),
                  lambda[overflow.idx],
                  sqrt(lambda[overflow.idx]))))
  return(x)
}

#' Simulate the number of urns.
#'
#' Simulate the number of non-empty urns when m balls placed into n urns.
#'
#' @param m the number of balls, single integer or vector of integers.
#' @param n the number of urns, single integer or vector of integers.
#' @param exact.n single integer, the maximum number of urns for which to use
#'   exact calculations instead of a normal approximation.
#' @return the simulated number of non-empty urns
#' @keywords internal

SimulateNotEmptyUrns <- function(m, n, exact.n = 20) {

  # Define a function that calculates the probability an individual urn is
  # empty.
  p.empty.each <- function(a, b) {
    (b != 0) * (1 - 1 / pmax(1, b)) ^ a  # approx exp(-a / b)
  }

  # Calculate the moments for the number of empty urns.
  n.empty.m1 <- n * p.empty.each(m, n)
  n.empty.m2 <- n.empty.m1 * (1 + (n - 1) * p.empty.each(m, n - 1))

  # Simulate the number of empty urns.
  n.empty <- mapply(
      function(m1, m2) rnorm(1, m1, sqrt(pmax(0, m2 - m1 ^ 2))),
      n.empty.m1, n.empty.m2)

  # Enforce hard bounds on possible number of non-empty urns.
  n.notempty <- pmin(n, m, pmax(m > 0, n - round(n.empty)))

  # Do exact calculations for cases with a small number of urns.
  small.n.idx <- (n >= 2) & (n <= exact.n)
  if (any(small.n.idx)) {
    n.notempty[small.n.idx] <-
        mapply(function(a, b) sum(RMultinom(1, a, rep(1 / b, b)) > 0),
               m[small.n.idx], n[small.n.idx])
  }
  return(n.notempty)
}
