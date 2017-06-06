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

set.seed(1)

GetArgs <- function(period.length = 10, pop.size = 1e6) {
  # Produces arguments to pass to SimulateAMSS().
  #
  # Args:
  #   period.length: changes the number of time periods in a "year" of
  #     simulation.
  #
  # Returns:
  #   arguments to pass to SimulateAMSS()

  time.n <- period.length * 4

  # Parameters controlling natural migration
  # a sinusoidal pattern
  market.rate.nonoise <-
      SimulateSinusoidal(4 * period.length, period.length,
                         vert.trans = 0.6, amplitude = 0.25)
  # with some added noise
  market.rate.seas <- pmax(
      0, pmin(1,
              market.rate.nonoise *
              SimulateAR1(length(market.rate.nonoise), 1, 0.1, 0.3)))
  activity.transition <- matrix(
      c(0.60, 0.30, 0.10,  # migration originating from inactive state
        0.60, 0.30, 0.10,  # exploratory state
        0.60, 0.30, 0.10),  # purchase state
      nrow = length(kActivityStates), byrow = TRUE)
  favorability.transition <- matrix(
      c(0.03, 0.07, 0.65, 0.20, 0.05,  # migration from the unaware state
        0.03, 0.07, 0.65, 0.20, 0.05,  # negative state
        0.03, 0.07, 0.65, 0.20, 0.05,  # neutral state
        0.03, 0.07, 0.65, 0.20, 0.05,  # somewhat favorable state
        0.03, 0.07, 0.65, 0.20, 0.05),  # favorable state
      nrow = length(kFavorabilityStates), byrow = TRUE)
  nat.mig.params <- list(
      population = pop.size,
      market.rate.trend = 0.68,
      market.rate.seas = market.rate.seas,
      # activity states for newly responsive (in-market & un-satiated)
      prop.activity = c(0.375, 0.425, 0.2),
      # brand favorability, initial proportions.
      prop.favorability = c(0.03, 0.07, 0.65, 0.20, 0.05),
      # everyone is a switcher
      prop.loyalty = c(1, 0, 0),
      transition.matrices = list(
          activity = activity.transition,
          favorability = favorability.transition))

  # Parameters controlling sales module
  sales.params <- list(
      competitor.demand.max = list(loyalty = c(0.8, 0, 0.8)),
      advertiser.demand.slope = list(favorability = rep(0, 5)),
      advertiser.demand.intercept = list(
          favorability = c(0.014, 0, 0.2, 0.3, 0.9)),
      price = 55)

  # media parameters
  tv.budget <- c(0, .005 * period.length, .01 * period.length, 0) * pop.size
  budget.index <- rep(1:4, each = period.length)
  tv.flighting <-
      pmax(0,
           market.rate.seas +
           SimulateAR1(length(market.rate.seas), -0.7, 0.7, -0.7))
  tv.flighting <- tv.flighting[c(3:length(tv.flighting), 1:2)]
  tv.activity.trans.mat <- matrix(
      c(0.99, 0.01, 0.00,  # migration originating from the inactive state
        0.00, 0.99, 0.01,  # exploratory state
        0.00, 0.00, 1.00),  # purchase state
      nrow = length(kActivityStates), byrow = TRUE)
  tv.favorability.trans.mat <- matrix(
      c(0.4,  0.0,  0.4, 0.2, 0.0,  # migration from the unaware state
        0.0,  0.9,  0.1, 0.0, 0.0,  # negative state
        0.0,  0.0,  0.5, 0.5, 0.0,  # neutral state
        0.0,  0.0,  0.0, 0.8, 0.2,  # somewhat favorable state
        0.0,  0.0,  0.0, 0.0, 1.0),  # favorable state
      nrow = length(kFavorabilityStates), byrow = TRUE)
  params.m1 <- list(
    audience.membership = list(activity = rep(0.5, 3)),
    budget = tv.budget,
    budget.index = budget.index,
    flighting = tv.flighting,
    unit.cost = 0.002,
    hill.ec = 1.56,
    hill.slope = 1,
    transition.matrices = list(
        activity = tv.activity.trans.mat,
        favorability = tv.favorability.trans.mat))

  # search
  cpc.min <- 0.8
  cpc.max <- 1.1
  spend.cap.fn <- function(time.index, budget, budget.index) {
    if ((time.index %% 3) > 0) {
      return(Inf)
    } else {
      return(0)
    }
  }
  bid.fn <- function(time.index, per.capita.budget, budget.index) {
    return(1.1)
  }
  kwl.fn <- function(time.index, per.capita.budget, budget.index) {
      return(per.capita.budget)
  }

  search.activity.trans.mat <- matrix(
      c(0.05, 0.95, 0.00,  # starting state: inactive
        0.00, 0.85, 0.15,  # starting state: exploratory
        0.00, 0.00, 1.00),  # starting: purchase
      nrow = length(kActivityStates), byrow = TRUE)
  search.favorability.trans.mat <- matrix(
      c(1.0, 0.0, 0.0, 0.0, 0.0,  # unaware
        0.0, 1.0, 0.0, 0.0, 0.0,  # negative
        0.0, 0.0, 1.0, 0.0, 0.0,  # neutral
        0.0, 0.0, 0.0, 1.0, 0.0,  # favorable
        0.0, 0.0, 0.0, 0.0, 1.0),  # loyal
      nrow = length(kFavorabilityStates), byrow = TRUE)
  params.m2 <- list(
      audience.membership = list(activity = c(0.01, 0.3, 0.4)),
      budget = (pop.size / 4.1) * (1:4),
      budget.index = budget.index,
      spend.cap.fn = spend.cap.fn,
      bid.fn = bid.fn,
      kwl.fn = kwl.fn,
      query.rate = 1,
      cpc.min = cpc.min,
      cpc.max = cpc.max,
      ctr = list(activity = c(0.005, 0.08, 0.10)),
      relative.effectiveness = c(0, 0.1, 1),
      transition.matrices = list(
          activity = search.activity.trans.mat,
          favorability = search.favorability.trans.mat))

  return(list(
      time.n = time.n,
      nat.mig.params = nat.mig.params,
      media.names = c("traditional", "search"),
      media.modules = c(`DefaultTraditionalMediaModule`,
                        `DefaultSearchMediaModule`),
      media.params = list(params.m1, params.m2),
      sales.params = sales.params))
}

GetData <- function(arglist) {
  sim.data <- do.call(SimulateAMSS, arglist)
  return(sim.data)
}

test.args <- GetArgs()
test.data <- GetData(test.args)
