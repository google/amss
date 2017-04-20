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

utils::globalVariables(c("pop", "revenue", "rep.index", "V1", "total.spend"))

#' Calculate ROAS or mROAS.
#'
#' This functions takes the original budget settings and a
#' counterfactual budget setting. It reports the expected ratio between
#' the total difference in revenue over all time points and the total
#' difference in media spend over all time points.
#'
#' @param object amss.sim object containing simulated data
#' @param new.budget table of new budgets for each budget period (row) and
#'   media channel (column)
#' @param media.names if new.budget is NULL, adjust original budget of the
#'   media named here.
#' @param budget.periods budget.periods over which to modify the budget.
#'   Default \code{NULL} will lead to all budget periods being modified.
#' @param budget.proportion nonnegative numeric. When \code{new.budget} is
#'   NULL, it is calculated by setting the budget of the media channels
#'   specified in \code{media.names} to \code{budget.proportion}
#'   proportion of the original budget during the budget periods specified
#'   in \code{budget.periods}. The default proportion of 0 is used to
#'   calculate the average ROAS over the entire spend in the channel.
#'   Values such as 0.99 can be used to calculate the marginal ROAS.
#' @param t.start time point to start generating data according to the new
#'   settings.
#' @param t.end last time point to generate data according to the new settings.
#'   In scenarios with lag, this should extend past the last time point in
#'   the modified budget periods in order to include lagged effects in the
#'   calculation.
#' @param scaled.pop.size \code{CalculateROAS} scales up the population size to
#'   reduce the variability of its estimates. This number should be chosen
#'   to be as large as possible while avoiding integer overflow during
#'   data simulation.
#' @param min.reps integer representing the initial number of datasets to
#'   generate from each budget setting. A reasonable number of initial
#'   datasets is needed to estimate the amount of variability accurately
#' @param max.coef.var numeric, the target coefficient of variation. The
#'   function takes additional samples of the ROAS until it runs out of
#'   time, attains the target coefficient of variation, or attains the
#'   target margin of error.
#' @param max.margin.error numeric, the target margin of error. The function
#'   takes additional samples of the ROAS until it runs out of time,
#'   attains the target coefficient of variation, or attains the target
#'   margin of error.
#' @param max.time numeric, the number of minutes at which to cut off the
#'   function from taking additional samples beyond the initial sample
#'   generated according to \code{min.reps}. The function takes additional
#'   samples of the ROAS until it runs out of time, attains the target
#'   coefficient of variation, or attains the target margin of error.
#' @param verbose boolean. If TRUE, output measures of the accuracy of the
#'   reported ROAS, including the full sample of ROAS values.
#' @return numeric value for ROAS, or, if \code{verbose = TRUE}, a list with
#'   the roas, the 95% margin of error, the coefficient of variation, and the
#'   sample ROAS values.
#' @export

CalculateROAS <- function(
    object,
    new.budget = NULL,
    media.names = object$params$media.names,
    budget.periods = NULL,
    budget.proportion = rep(0, length(media.names)),
    t.start = 1,
    t.end = object$params$time.n,
    scaled.pop.size = .Machine$integer.max / 100,
    min.reps = 10,
    max.coef.var = 0.01, max.margin.error = 0.01, max.time = 30,
    verbose = FALSE) {

  if (is.null(object$params$media.names) ||
      length(object$params$media.names) == 0) {
    warning("Cannot calculate ROAS: no media channels in this simulation.")
    return(NULL)
  }

  # Check that the object is an AMSS simulation object.
  assertthat::assert_that(inherits(object, "amss.sim"))

  # Get the original budget settings.
  orig.budget <- .GetBudget(object)

  # Check the other parameters.
  if (!is.null(new.budget)) {
    assertthat::assert_that(dim(new.budget) == dim(orig.budget))
  }
  assertthat::assert_that(all(media.names %in% object$params$media.names))
  assertthat::assert_that(all(budget.periods >= 1))
  assertthat::assert_that(all(budget.periods <= nrow(orig.budget)))
  assertthat::assert_that(all(budget.proportion >= 0))
  assertthat::assert_that(t.start >= 1)
  assertthat::assert_that(t.end >= t.start)
  assertthat::assert_that(scaled.pop.size >= 1)
  assertthat::assert_that(min.reps >= 1)
  assertthat::assert_that(assertthat::is.flag(verbose))

  # Set the new budget.
  if (missing(new.budget) || is.null(new.budget)) {
    new.budget <- orig.budget
    if (missing(budget.periods) || is.null(budget.periods)) {
      budget.periods <- 1:nrow(new.budget)
    }
    for (iter.media in 1:length(media.names)) {
      new.budget[budget.periods, media.names[iter.media]] <-
          new.budget[budget.periods, media.names[iter.media]] *
          budget.proportion[iter.media]
    }
  }

  # Modified the object with an updated population size, to reduce runtime.
  # Note that, since only values in column "pop" affect the values in the
  # regenerated data. That is the only column that needs updating.
  scaled.pop.size <- as.integer(scaled.pop.size)
  orig.pop <- .GetPopulation(object)
  pop.multiplier <- scaled.pop.size / orig.pop
  mod.object <- data.table::copy(object)
  lapply(mod.object$data.full,
         function(dt) dt[, pop := .AdjustPopulation(pop, scaled.pop.size)])
  mod.object$params$nat.mig.params$population <- scaled.pop.size

  # Generate data from counterfactual budget settings.
  start.time <- Sys.time()
  new.data <- .GenerateDataUnderNewBudget(
      mod.object, new.budget * pop.multiplier,
      reps = min.reps, t.start = t.start, t.end = t.end)
  # Generate more data from the original budget settings to average out noise.
  orig.data <- .GenerateDataUnderNewBudget(
      mod.object, orig.budget * pop.multiplier,
      reps = min.reps, t.start = t.start, t.end = t.end)

  # Calculate the ROAS based on the current sample.
  roas.sample <- .CalculateSampleROAS(new.data, orig.data)
  # Get the estimate.
  roas.est <- mean(roas.sample)
  # Get its precision as a standard error.
  if (min.reps < 2) {
    roas.standard.error <- Inf
  } else {
    roas.standard.error <- sd(roas.sample) / sqrt(length(roas.sample))
  }
  # Get the margin of error.
  roas.margin.error <- roas.standard.error * qt(0.975, length(roas.sample))
  # Get the coefficient of variation.
  roas.coef.var <- roas.standard.error / abs(roas.est)

  # While time is available and the requested precision has not yet been
  # achieved, generate additional samples.
  counter <- min.reps + 1
  while(roas.margin.error > max.margin.error &&
        roas.coef.var > max.coef.var) {
    if (difftime(Sys.time(), start.time, units = "mins") > max.time) {
      warning("Warning: requested precision not achieved.")
      break
    }
    print(paste("Running additional sample", counter, "for more accuracy."))
    print(paste(round(difftime(Sys.time(), start.time, units = "mins")),
                "minutes elapsed."))
    new.data.1 <- .GenerateDataUnderNewBudget(
      object = mod.object, new.budget = new.budget * pop.multiplier,
      t.start = t.start, t.end = t.end)
    orig.data.1 <- .GenerateDataUnderNewBudget(
        object = mod.object, new.budget = orig.budget * pop.multiplier,
        t.start = t.start, t.end = t.end)
    roas.sample <- c(roas.sample,
                     .CalculateSampleROAS(new.data.1, orig.data.1))
    roas.est <- mean(roas.sample)
    roas.standard.error <- sd(roas.sample) / sqrt(length(roas.sample))
    roas.margin.error <- roas.standard.error * qt(0.975, length(roas.sample))
    roas.coef.var <- roas.standard.error / abs(roas.est)
    counter <- counter + 1
  }

  # Return the ROAS, plus precision information if verbose = TRUE.
  if (verbose) {
    return(list(roas = roas.est,
                margin.error = roas.margin.error,
                coef.var = roas.coef.var,
                sample = roas.sample))
  } else {
    return(roas.est)
  }
}

#' Calculate ROAS from a pair of datasets.
#'
#' Calculate the ROAS estimated from a sample of datasets generated from two
#' budget settings
#'
#' @param dt1 data.table of data generated from the first budget setting. The
#'   data.table must include "revenue" and "total.spend" columns, and have
#'   the different datasets indexed by column "rep.index."
#' @param dt2 data.table of data generated from the second budget setting. The
#'   data.table must include "revenue" and "total.spend" columns, and have
#'   the different datasets indexed by column "rep.index." the number of
#'   unique values of "rep.index" in \code{dt1} and \code{dt2} should
#'   match.
#' @return vector of ROAS estimates from each pair of sample datasets in dt1
#'   and dt2.

.CalculateSampleROAS <- function(dt1, dt2) {

  return((dt1[, sum(revenue), by = rep.index][, V1] -
          dt2[, sum(revenue), by = rep.index][, V1]) /
         (dt1[, sum(total.spend), by = rep.index][, V1] -
          dt2[, sum(total.spend), by = rep.index][, V1]))
}

#' Generate data under a new budget
#'
#' Simulate multiple datasets from a counterfactual simulation setting with
#' new budget settings.
#'
#' @param object object of class \code{amss.sim} to do prediction on.
#' @param new.budget new budget levels for each media
#' @param response.metric string specifying what observable value to predict.
#'   defaults to NULL, which results in entire dataset. Values such as
#'   "log(brand.sales)" will make the prediction function return the
#'   average log(brand.sales) over all reps, for ex.
#' @param reps number of replicates to generate
#' @param t.start time point at which to start generating new data
#' @param t.end time point at which to stop generating new data
#' @return The list of all generated datasets or a vector of averaged values.

.GenerateDataUnderNewBudget <- function(
    object,
    new.budget = .GetBudget(object),
    response.metric = NULL,
    reps = 1,
    t.start = 1,
    t.end = object$params$time.n) {

  if (is.null(object$params$media.names) ||
      length(object$params$media.names) == 0) {
    warning("No media channels to adjust")
    new.params <- c(list(starting.dts = list()),
                     object$params)
  } else {
    # Keep original data from before start time.
    if (t.start > 1) {
      starting.dts <- data.table::copy(object$data.full[1:(t.start - 1)])
    } else {
      starting.dts <- list()
    }
    new.params <- c(
        list(starting.dts = starting.dts),
        object$params)
    # Adjust data generating end time.
    new.params$time.n <- t.end
    # Adjust media budgets.
    for (iter.media in 1:length(new.params$media.params)) {
      new.params$media.params[[iter.media]]$budget <- new.budget[, iter.media]
    }
  }

  # Generate data.
  new.data <- lapply(
      1:reps,
      function(r) .SurfaceData(
          do.call(`.SimulateData`, new.params[formalArgs(`.SimulateData`)]),
          new.params$names.agg.const,
          new.params$names.agg.sum)[, rep.index := r])
  new.data <- rbindlist(new.data)
  if (is.null(response.metric)) {
    return(new.data)
  } else {
    return(new.data[,
                    eval(.ParseT(paste0("mean(", response.metric, ")"))),
                    by = c("time.index", "geo.index")][, V1])
  }
}

#' Adjust population vector to a new total
#'
#' Adjust population proportionally to a new total population size, rounding
#' by the largest remainder method
#'
#' @param orig.pop original population vector
#' @param new.total.pop new total population size
#' @return new population vector of class integer

.AdjustPopulation <- function(orig.pop, new.total.pop) {

  pop.multiplier <- new.total.pop / sum(orig.pop)
  precise.pop <- orig.pop * pop.multiplier
  quotient.pop <- floor(precise.pop)
  remainder.pop <- precise.pop - quotient.pop
  round.up.idx <- order(
      remainder.pop,
      decreasing = TRUE)[1:(new.total.pop - sum(quotient.pop))]
  return(as.integer(quotient.pop + (1:length(quotient.pop) %in% round.up.idx)))
}
