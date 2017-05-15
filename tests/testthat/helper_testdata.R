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

GetArgs <- function(period.length = 10) {
  # Produces arguments to pass to SimulateAMSS().
  #
  # Args:
  #   period.length: changes the number of time periods in a "year" of
  #     simulation. A total of 4 years is simulated.
  #
  # Returns:
  #   arguments to pass to SimulateAMSS()

  time.n <- period.length * 4

  # Parameters controlling natural migration
  market.rate.trend <-
      pmin(1,
           pmax(0,
                SimulateAR1(time.n, 0.75, 0.05, 0.3)))
  market.rate.seas <-
      pmin(1,
           pmax(0,
                SimulateSinusoidal(
                    n = time.n, period = period.length / 2, max.loc = 1,
                    vert.translation = 0.5, amplitude = 0.4)))
  favorability.transition <-
      matrix(c(0.90, 0.02, 0.05, 0.03, 0.00,  # unaware
               0.00, 0.90, 0.10, 0.00, 0.00,  # negative
               0.00, 0.10, 0.75, 0.15, 0.00,  # neutral
               0.00, 0.00, 0.10, 0.75, 0.15,  # somewhat favorable
               0.00, 0.00, 0.00, 0.20, 0.80),  # favorable
             5, byrow = TRUE)
  nat.mig.params <- list(
      population = 1000,
      market.rate.seas = market.rate.seas,
      market.rate.trend = market.rate.trend,
      sat.decay = 0.4,
      prop.activity = c(0.5, 0.3, 0.2),
      prop.favorability = c(0.15, 0.1, 0.2, 0.35, 0.2),
      transition.matrices = list(
          activity = matrix(0.1, 3, 3) + 0.7 * diag(3),
          favorability = favorability.transition))

  # Parameters controlling sales module
  sales.params <- list(
      competitor.demand.max = list(
          favorability = rep(0.8, 5),
          loyalty = c(1, 0, 1)),
      advertiser.demand.slope = list(
          favorability = c(0.04, 0.05, 0.03, 0.02, 0.01)),
      advertiser.demand.intercept = list(
          favorability = c(0.4, 0.2, 0.6, 0.8, 1)),
      price = 15)

  # media parameters
  budget <- c(0, 250 * period.length, 500 * period.length, 0)
  budget.index <- rep(1:4, each = period.length)
  flighting <- pmax(
      SimulateCorrelated(
          v = market.rate.seas, cor.vx = 0.8,
          mu.x = mean(market.rate.seas), sigma.x = sd(market.rate.seas)),
      0)
  activity.trans.mat <- matrix(
      c(0.50, 0.35, 0.15,  # starting state: inactive
        0.00, 0.70, 0.30,  # starting state: exploratory
        0.00, 0.00, 1.00),  # starting: purchase
      length(kActivityStates), length(kActivityStates), TRUE)
  favorability.trans.mat <- matrix(
      c(0.3, 0.0, 0.50, 0.20, 0.0,  # unaware
        0.0, 1.0, 0.00, 0.00, 0.0,  # negative
        0.0, 0.0, 0.75, 0.25, 0.0,  # neutral
        0.0, 0.0, 0.00, 1.00, 0.0,  # somewhat.favorable
        0.0, 0.0, 0.00, 0.00, 1.0),  # favorable
      length(kFavorabilityStates), length(kFavorabilityStates), TRUE)
  params.m1 <- list(
      audience.membership = list(activity = rep(0.4, 3)),
      budget = budget,
      budget.index = budget.index,
      flighting = flighting,
      unit.cost = 0.1,
      hill.ec = 0.3,
      hill.slope = 4,
      transition.matrices = list(
          activity = activity.trans.mat,
          favorability = favorability.trans.mat))

  # search
  search.activity.trans.mat <- matrix(
      c(0.50, 0.35, 0.15,  # starting state: inactive
        0.00, 0.70, 0.30,  # starting state: exploratory
        0.00, 0.00, 1.00),  # starting: purchase
      length(kActivityStates), length(kActivityStates), TRUE)
  search.favorability.trans.mat <- matrix(
      c(0.3, 0.0, 0.50, 0.20, 0.0,  # unaware
        0.0, 1.0, 0.00, 0.00, 0.0,  # negative
        0.0, 0.0, 0.75, 0.25, 0.0,  # neutral
        0.0, 0.0, 0.00, 1.00, 0.0,  # favorable
        0.0, 0.0, 0.00, 0.00, 1.0),  # loyal
       length(kFavorabilityStates), length(kFavorabilityStates), TRUE)
  params.m2 <- list(
      audience.membership = list(
          activity = rep(0.4, 3),
          favorability = rep(1, 5)),
      budget = c(0, 250 * period.length, 500 * period.length, 0),
      budget.index = rep(1:4, each = period.length),
      spend.cap.fn = function(time.index, budget, budget.index) {
        return(budget / period.length * 1.4)},
      bid.fn = function(time.index, per.capita.budget, budget.index)
          return(1.2 * HillTrans(per.capita.budget, 0.01, 4)),
      kwl.fn = function(time.index, per.capita.budget, budget.index) {
        kwll <- sum(per.capita.budget >=
                    c(0, 0.021, 0.061, 0.081, 0.121, 0.161, 0.221))
        return(c(0, 2000, 6000, 8000, 12000, 16000, 22000)[kwll] / 22000)
      },
      query.rate = 1,
      cpc.min = 0.8,
      cpc.max = 1.4,
      ctr = list(
          activity = c(0.02, 0.6, 0.8),
          favorability = c(0.15, 0, 0.3, 0.6, 1)),
      relative.effectiveness = c(0.1, 0.2, 1),
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
