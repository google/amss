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

utils::globalVariables(c("pop", "satiation", ".", "market", "time.index"))

#' Model natural consumer behavior in the absence of marketing interventions.
#'
#' This function models natural consumer behavior in the absence of marketing
#' interventions. In particular, it models changes in consumer mindset over
#' time that are outside of advertiser control, such as seasonal changes.
#'
#' @param data.dt data.table with rows corresponding to segments and columns
#'   corresponding to variables; column \code{pop} for the number of
#'   people in each segment must be included.
#' @param population constant specifying population size
#' @param market.rate.trend the trend in market size, written as the proportion
#'   of the population to be considered potentially in the market, pending
#'   seasonal adjustments. If a vector, should match time.n in length.
#'   Defaults to 1, for full population participation in market.
#' @param market.rate.seas the seasonal variation in market size, written as
#'   the proportion of the post-market-trend population in the market. For
#'   example, for market.rate.trend = 0.8 and market.rate.seas = 0.5,
#'   seasonal variation leaves 40% = 50% of the 80% of the population
#'   potentially in market according to market.rate.trend actually in
#'   market. If a vector, should match time.n in length. Defaults to 1 for
#'   full population participation in market.
#' @param sat.decay single numeric value between 0 and 1, representing the
#'   geometric decay rate at which satiated individuals become unsatiated.
#'   Defaults to 1 for satiation lasting 1 time period for all
#'   individuals.
#' @param prop.activity vector of nonnegative values summing to 1, representing
#'   the proportion of the population to be assigned to each activity
#'   state, given they are "responsive," i.e., "in.market" and
#'   "unsatiated."
#' @param prop.favorability vector of nonnegative values summing to 1,
#'   representing the proportion of the population to be assigned to each
#'   favorability state, given they are not "loyal."
#' @param prop.loyalty vector of nonnegative values summing to 1, representing
#'   the proportion of the population to be assigned to each loyalty
#'   state.
#' @param prop.availability vector of nonnegative values summing to 1,
#'   representing the proportion of the population to be assigned to each
#'   availability state.
#' @param transition.matrices list of matrices for each dimension of population
#'   segmentation that may be affected by marketing interventions. A named
#'   list with members 'activity', 'favorability', 'loyalty', and
#'   'availability' is expected. By default, any missing members will have
#'   no effect. The transition matrices represent natural migration in
#'   these dimensions, and control how quickly the population returns to
#'   its equilibrium allocation across segments after marketing
#'   interventions.
#' @return invisible(NULL). data.dt is updated by reference.
#' @export

DefaultNatMigModule <- function(
    data.dt,
    population,
    market.rate.trend = 1, market.rate.seas = 1,
    sat.decay = 1,
    prop.activity = rep(1 / length(kActivityStates), length(kActivityStates)),
    prop.favorability = rep(1 / length(kFavorabilityStates),
                              length(kFavorabilityStates)),
    prop.loyalty = rep(1 / length(kLoyaltyStates), length(kLoyaltyStates)),
    prop.availability = rep(1 / length(kAvailabilityStates),
                              length(kAvailabilityStates)),
    transition.matrices = list()) {

  # Update time index.
  data.dt[, time.index := time.index + 1]
  curr.time <- data.dt[1, time.index]

  # Check parameters.
  if (curr.time == 1) {
    assertthat::assert_that(is.numeric(population), length(population) == 1,
                            population > 0)
    assertthat::assert_that(is.numeric(market.rate.trend),
                            is.numeric(market.rate.seas))
    assertthat::assert_that(is.numeric(sat.decay), length(sat.decay) == 1,
                            sat.decay >= 0, sat.decay <= 1)
    for (iter.dim in setdiff(colnames(kAllStates), c("market", "satiation"))) {
      curr.prop <- get(.PasteD("prop", iter.dim))
      curr.states <- get(paste0("k", .Capitalize(iter.dim), "States"))
      assertthat::assert_that(
          is.numeric(curr.prop), length(curr.prop) == length(curr.states),
          all(curr.prop >= 0),  all(curr.prop <= 1))
    }
    .CheckListNames(transition.matrices)
  }

  # Calculate market rate.
  if (max(length(market.rate.trend), length(market.rate.seas)) == 1) {
    market.rate <- market.rate.trend * market.rate.seas
  } else {
    market.rate.trend <- rep(market.rate.trend, length = curr.time)
    market.rate.seas <- rep(market.rate.seas, length = curr.time)
    market.rate <- market.rate.trend[curr.time] *
        market.rate.seas[curr.time]
    assertthat::assert_that(market.rate >= 0, market.rate <= 1)
  }

  # If this is the first timepoint, initialize population.
  if (curr.time == 1) {
    .InitPop(data.dt, population, market.rate,
             prop.activity, prop.favorability,
             prop.loyalty, prop.availability)
  }
  # Update each dimension of population segmentation, one at a time.
  .UpdateMarket(data.dt, market.rate, prop.activity)
  .Desatiate(data.dt, sat.decay, prop.activity)
  .UpdateMarketingResponsiveStates(data.dt, transition.matrices)

  return(invisible(NULL))
}

#' Initialize population segmentation.
#'
#' @param data.dt data.table containing all state-related data
#' @param pop.total total population
#' @param market.rate target proportion of consumers in 'in-market' market
#'   state.
#' @param prop.activity vector of nonnegative values summing to 1, representing
#'   the proportion of the population to be assigned to each activity
#'   state, given they are "responsive," i.e., "in.market" and
#'   "unsatiated."
#' @param prop.favorability vector of nonnegative values summing to 1,
#'   representing the proportion of the population to be assigned to each
#'   favorability state, given they are not "loyal."
#' @param prop.loyalty vector of nonnegative values summing to 1, representing
#'   the proportion of the population to be assigned to each loyalty
#'   state.
#' @param prop.availability vector of nonnegative values summing to 1,
#'   representing the proportion of the population to be assigned to each
#'   availability state.
#' @return invisible(NULL). data.dt is updated as a side effect of this
#'   function.

.InitPop <- function(
    data.dt, pop.total,
    market.rate = 1,
    prop.activity = rep(1 / length(kActivityStates), length(kActivityStates)),
    prop.favorability = rep(1 / length(kFavorabilityStates),
                            length(kFavorabilityStates)),
    prop.loyalty = rep(1 / length(kLoyaltyStates),
                       length(kLoyaltyStates)),
    prop.availability = rep(1 / length(kAvailabilityStates),
        length(kAvailabilityStates))) {

  # Check input.
  stopifnot(data.dt[, sum(pop)] == 0)

  # Start everyone together in one state.
  # Everyone is unsatiated.
  # Start as favorable so that prop.loyalty will be applied to everyone.
  # Start out of market so that `UpdateMarket` will assign market and
  # activity states to everyone in specified proportions.
  data.dt[.("out.market", "unsatiated", "inactive",
            "favorable", "switcher", "average"),
          pop := pop.total]
  .UpdateMarket(data.dt, market.rate, prop.activity)
  .MigrateMultiple(data.dt, data.dt[, pop],
                   c("loyalty", "favorability", "availability"),
                   list(matrix(prop.loyalty, length(kLoyaltyStates),
                               length(kLoyaltyStates), TRUE),
                        matrix(prop.favorability, length(kFavorabilityStates),
                               length(kFavorabilityStates), TRUE),
                        matrix(prop.availability, length(kAvailabilityStates),
                               length(kAvailabilityStates), TRUE)))
  return(invisible(NULL))
}

#' Updates in/out of market status by moving mimimal population in/out of the
#' market necessary to match on requested proportion of "in.market"
#' individuals.
#'
#' @param data.dt data.table containing all state-related data
#' @param target.rate target proportion of consumers 'in-market'
#' @param prop.activity single value between 0 and 1, representing proportion
#'   of population to be assigned to each activity state, given they are
#'   "responsive," i.e., "in.market" and "unsatiated."
#' @return invisible(NULL). data.dt is updated by reference.

.UpdateMarket <- function(
    data.dt, target.rate,
    prop.activity = rep(1 / length(kActivityStates), length(kActivityStates))) {

  # Calculate current market rate
  current.rate <- data.dt[market == "in.market", sum(pop)] / data.dt[, sum(pop)]

  # Case: target.rate > current.rate, so move population from "out.of.market"
  # to "in.market" state.
  if (target.rate > current.rate) {
    mig.rate <- (target.rate - current.rate) / (1 - current.rate)
    .MigrateMultiple(
        data.dt, data.dt[, pop * (market == "out.market")],
        c("market", "activity"),
        list(matrix(c(1 - mig.rate, mig.rate, 0, 1), 2, 2, TRUE),
             matrix(prop.activity, 3, 3, TRUE)))
  }

  # Case: target.rate < current.rate, so move population from "in.market" to
  # "out.of.market" state.
  if (target.rate < current.rate){
    mig.rate <- (current.rate - target.rate) / current.rate
    .MigrateMarginal(
        data.dt, data.dt[, pop * (market == "in.market")],
        "market",
        matrix(c(1, 0,
                 mig.rate, 1 - mig.rate),
               2, 2, TRUE))
  }
  return(invisible(NULL))
}

#' Implement desatiation.
#'
#' @param data.dt data.table to update
#' @param sat.decay rate for the geometric decay of satiation
#' @param prop.activity proportion of population assigned to each activity
#'   state, given that they are responsive.
#' @return invisible(NULL). data.dt is updated by reference.
#'
#' @note
#' Satiation has geometric decay with rate \code{sat.decay}. Thus,
#' during any time interval, \code{sat.decay} proportion of currently
#' satiated individuals desatiate. Individuals put into a responsive state
#' by desatiation are assigned activity states at rates defined by
#' prop.activity.

.Desatiate <- function(
    data.dt, sat.decay,
    prop.activity = rep(1 / length(kActivityStates), length(kActivityStates))) {

  # Currently satiated individuals become unsatiated with probability
  # "sat.decay". If the desatiated individuals were "in.market", their activity
  # state also updates.
  .MigrateMultiple(
      data.dt, data.dt[, pop * (satiation == "satiated")],
      c("satiation", "activity"),
      list(matrix(c(1 - sat.decay, sat.decay, 0, 1), 2, 2, TRUE),
           matrix(prop.activity, 3, 3, TRUE)))
  return(invisible(NULL))
}

#' Update segmentation in marketing-responsive states (activity,
#' favorability, loyalty, and availability) according to specified
#' transition matrices.
#'
#' @param data.dt data.table with column 'pop' for population segment size.
#' @param transition.matrices list of transition matrices for each dimension of
#'   population segmentation that may be affected by marketing
#'   interventions. A named list with members 'activity', 'favorability',
#'   'loyalty', and 'availability' is expected. By default, any missing
#'   members will have no effect.
#' @return invisible(NULL). data.dt is updated by reference.

.UpdateMarketingResponsiveStates <- function(
    data.dt, transition.matrices = list()) {

  # Check input.
  .CheckListNames(transition.matrices,
                  setdiff(colnames(kAllStates), c("market", "satiation")))

  # Perform migration.
  .MigrateMultiple(data.dt, data.dt[, pop],
                   names(transition.matrices),
                   transition.matrices)
  return(invisible(NULL))
}
