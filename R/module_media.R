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

utils::globalVariables(c(
    "time.index", "pop", "audience", "clicks", "imps",
    "matching.query.volume", "query.volume", "spend", "volume",
    "absolute.reach"))

#' Model the effect of a traditional media channel.
#'
#' Simulate the behavior of a traditional media channel, and generate
#' associated observable variables such as media volume and spend.
#'
#' @param data.dt data.table with rows corresponding to population segments and
#'   columns corresponding to specific variables
#' @param budget.index vector specifying budget period each time point belongs
#'   to. For example, rep(1:4, each = 52) would correspond to 4 years of
#'   yearly budget periods.
#' @param budget vector specifying the target spend for each budget period. For
#'   example, given the example \code{budget.index} from above,
#'   \code{budget = rep(1e6, 4)} would specify a budget of 1 million for
#'   each year.
#' @param audience.membership list of multipliers used to calculate probability
#'   of audience membership. Each element of the list corresponds to a
#'   specific dimension of population segmentation. Multipliers
#'   corresponding to each dimension are multiplied to derive audience
#'   membership probability for each segment. A named list with members
#'   'activity', 'favorability', 'loyalty', and 'availability' is
#'   expected. Each member is a numeric vector containing the multipliers
#'   to use for each state in the dimension. For example, if member
#'   "activity" is c(1, 0.5, 0.7), a multiplier of 0.7 should be used for
#'   all segments with activity state "purchase." By default, any missing
#'   members will have no effect.
#' @param flighting specifies the relative amount to be spent on each time
#'   point within a budget period. For example, in a budget period of two
#'   weeks, \code{flighting = c(1,2)} specifies that twice 1/3 of the
#'   budget should be spent in the first week, and 2/3 in the second.
#' @param unit.cost positive numeric specifying expected unit cost per
#'   exposure.
#' @param effectiveness.function vectorized function mapping frequency to media
#'   effect (relative to transition matrices specifying maximum effect).
#'   The range of the function should be bounded between 0 and 1. Given
#'   the default value of NULL, the module will used the Hill
#'   transformation with parameters hill.ec and hill.slope.
#' @param hill.ec parameter controlling the scaling of frequency vs. effect.
#'   This is the EC50 of the Hill transformation.
#' @param hill.slope parameter controlling the scaling of frequency vs. effect.
#'   This is the maximum slope of the Hill transformation.
#' @param transition.matrices list of transition matrices for each dimension of
#'   population segmentation that may be affected by marketing
#'   interventions. A named list with members 'activity', 'favorability',
#'   'loyalty', and 'availability' is expected. By default, any missing
#'   members will have no effect.
#' @return invisible(NULL). \code{data.dt} updated by reference.
#' @export

DefaultTraditionalMediaModule <- function(
    data.dt,
    budget.index, budget,
    audience.membership = list(),
    flighting = rep(1, length(budget.index)),
    unit.cost = 1,
    effectiveness.function = NULL,
    hill.ec = 1, hill.slope = 1,
    transition.matrices = list()) {

  # Setup.
  fn.env <- environment()
  current.time <- data.dt[1, time.index]

  # Check parameters.
  if (current.time == 1) {
    assertthat::assert_that(is.numeric(budget), all(budget >= 0),
                            all(budget < Inf))
    assertthat::assert_that(is.numeric(flighting), all(flighting >= 0))
    assertthat::assert_that(is.numeric(unit.cost), all(unit.cost > 0))
    .CheckListNames(transition.matrices,
                    setdiff(colnames(kAllStates), c("market", "satiation")))
  }

  # Calculate the budget.
  assertthat::assert_that(length(budget.index) >= current.time)
  curr.budget.index <- budget.index[current.time]
  data.dt[, budget.index := fn.env$curr.budget.index]
  assertthat::assert_that(length(budget) >= max(budget.index))
  curr.budget <- budget[curr.budget.index]
  data.dt[, budget := fn.env$curr.budget]

  # Simulate the audience size.
  data.dt[,
          audience := rbinom(length(pop), pop,
                             .MultiplyBySegment(audience.membership))]

  # Simulate the amount of spend and the volume of exposures.
  assertthat::assert_that(length(flighting) == length(budget.index))
  assertthat::assert_that(all(flighting >= 0))
  flighting <- flighting[current.time] /
      sum(flighting[budget.index == budget.index[current.time]])
  if (curr.budget == 0) {
    total.spend <- 0
  } else {
    total.spend <- curr.budget * flighting
    assertthat::assert_that(
        !is.nan(total.spend),
        msg = paste("When the budget is non-zero, flighting must be",
                    "nonzero for at least one time interval."))
  }
  # Calculate the expected number of per-capita exposures.
  if (data.dt[, sum(audience)] > 0) {
    expected.percapita.volume <- total.spend / unit.cost /
        sum(data.dt[, audience])
  } else {
    expected.percapita.volume <- 0
  }
  # Generate the number of exposures
  data.dt[,
          volume := rpois(length(audience),
                          fn.env$expected.percapita.volume * audience)]
  total.volume <- data.dt[, sum(as.numeric(volume))]
  if (total.volume > 0) {
    data.dt[, spend := fn.env$total.spend * volume / total.volume]
  } else {
    data.dt[, spend := 0]
  }

  # Calculate the absolute reach (number of individuals reached) and the
  # average frequency of exposure for each segment.
  data.dt[, absolute.reach := .SimulateNotEmptyUrns(volume, audience)]
  data.dt[, frequency := ifelse(absolute.reach == 0,
                                0, volume / absolute.reach)]

  # Simulate migration between segments.
  if (is.null(effectiveness.function)) {
    effectiveness.function <- function(x) {
      HillTrans(x, hill.ec, hill.slope)
    }
  }
  effect.size <- data.dt[, effectiveness.function(frequency)]
  assertthat::assert_that(all(effect.size >= 0 & effect.size <= 1))
  migrating.pop <-
      rbinom(nrow(data.dt), data.dt[, absolute.reach], effect.size)
  .MigrateMultiple(data.dt, migrating.pop,
                   names(transition.matrices), transition.matrices)
  return(invisible(NULL))
}

#' Model paid and/or organic search.
#'
#' Simulate the behavior of a paid and/or organic search, including observable
#' variables (e.g., query volume, paid clicks, spend) and the effect on
#' consumer mindset.
#'
#' @param data.dt data.table with rows corresponding to population segments and
#'   columns corresponding to specific variables
#' @param budget.index vector specifying budget period each time point belongs
#'   to. For example, rep(1:4, each = 52) would correspond to 4 years of
#'   yearly budget periods.
#' @param budget vector specifying the target spend for each budget period. For
#'   example, given the example \code{budget.index} from above,
#'   \code{budget = rep(1e6, 4)} would specify a budget of 1 million for
#'   each year.
#' @param spend.cap.fn function mapping the current time, the budget, and the
#'   budget period to a spend cap for the current week. By default this is
#'   set to \code{Inf}, representing uncapped spend.
#' @param bid.fn function mapping the current time, the per-capita budget over
#'   the population, and the budget period to a bid for the current week.
#'   By default this is set to \code{Inf}, so that the advertiser wins all
#'   auctions and will pay the maximum CPC.
#' @param kwl.fn function mapping the current time, the per-capita budget over
#'   the population, and the budget period to the proportion of queries.
#'   that match the keyword list. By default this is the maximum value of
#'   1. To specify the proportion of matching queries by population
#'   segment, have kwl.fn return a vector with entries for each segment.
#' @param audience.membership list of multipliers used to calculate probability
#'   of audience membership. Each element of the list corresponds to a
#'   specific dimension of population segmentation. Multipliers
#'   corresponding to each dimension are multiplied to derive audience
#'   membership probability for each segment. A named list with members
#'   'activity', 'favorability', 'loyalty', and 'availability' is
#'   expected. Each member is a numeric vector containing the multipliers
#'   to use for each state in the dimension. For example, if member
#'   "activity" is c(1, 0.5, 0.7), a multiplier of 0.7 should be used for
#'   all segments with activity state "purchase." By default, any missing
#'   members will have no effect.
#' @param query.rate nonnegative numeric, or vector. Each member of the
#'   audience makes matching queries according to a Poisson process with
#'   this rate. A vector rate specifies the query rate at each time. Note
#'   that rate is the expected number of queries per person in the
#'   audience. Defaults to 1. Vector repeats as necessary, so that
#'   repeating patterns can be specified more simply.
#' @param cpc.min minimum CPC, defaults to 1. Must be nonnegative. vector
#'   values are interpreted as the vector of minimum CPC's over time.
#' @param cpc.max maximum CPC. Must be at least as large as cpc.min. vector
#'   values are interpreted as the vector of maximum CPC's over time.
#' @param ctr list of multipliers for each dimension with an effect on the
#'   clickthrough rate (ctr). Values in each state are multiplied to
#'   derive the ctr for each population segment. A named list with members
#'   'activity', 'favorability', 'loyalty', and 'availability' is
#'   expected. Each member is a numeric vector of the values for each
#'   state in that dimension. By default, any missing members will have no
#'   effect.
#' @param relative.effectiveness effectiveness, relative to the maximum
#'   effectiveness specified by the transition matrices, by volume type:
#'   organic only, paid impressions w/o paid click (click on organic
#'   result included), and paid clicks. Default to maximum (1)
#'   effectiveness for paid clicks, and no effect otherwise.
#' @param transition.matrices list of transition matrices for each dimension of
#'   population segmentation that may be affected by marketing
#'   interventions. A named list with members 'activity', 'favorability',
#'   'loyalty', and 'availability' is expected. By default, any missing
#'   members will have no effect.
#' @return invisible(NULL). \code{data.dt} updated by reference.
#' @export

DefaultSearchMediaModule <- function(
    data.dt,
    budget.index, budget,
    spend.cap.fn = function(time, budget, budget.indices) {Inf},
    bid.fn = function(time, per.capita.budget, budget.indices) {Inf},
    kwl.fn = function(time, per.capita.budget, budget.indices) {1},
    audience.membership = list(),
    query.rate = 1,
    cpc.min = 0, cpc.max = 1,
    ctr = list(),
    relative.effectiveness = c(0, 0, 1),
    transition.matrices = list()) {

  # Setup.
  fn.env <- environment()
  current.time <- data.dt[1, time.index]

  # Check parameters.
  if (current.time == 1) {
    assertthat::assert_that(is.numeric(budget), all(budget >= 0))
    assertthat::assert_that(is.numeric(query.rate), all(query.rate >= 0))
    assertthat::assert_that(is.numeric(cpc.min), all(cpc.min >= 0))
    assertthat::assert_that(is.numeric(cpc.max), all(cpc.max >= cpc.min))
    assertthat::assert_that(
        is.numeric(relative.effectiveness),
        length(relative.effectiveness) == 3,
        all(relative.effectiveness >= 0),
        all(relative.effectiveness <= 1))
    .CheckListNames(transition.matrices,
                    setdiff(colnames(kAllStates), c("market", "satiation")))
  }

  # Calculate budget variables.
  assertthat::assert_that(length(budget.index) >= current.time)
  curr.budget.index <- budget.index[current.time]
  data.dt[, budget.index := fn.env$curr.budget.index]
  assertthat::assert_that(length(budget) >= max(budget.index))
  curr.budget <- budget[curr.budget.index]
  data.dt[, budget := fn.env$curr.budget]

  # Get the campaign settings from the budget-to-campaign settings relationship.
  # Weekly spend cap.
  spend.cap <- spend.cap.fn(current.time, curr.budget, budget.index)
  # Advertiser bid.
  bid <- bid.fn(
      current.time, curr.budget / data.dt[, sum(pop)], budget.index)
  # Proportion of queries matching the keyword list.
  kwl <- kwl.fn(
      current.time, curr.budget / data.dt[, sum(pop)], budget.index)
  assertthat::assert_that(kwl >= 0, kwl <=1)
  # Check that matching probability is either single numeric or vector with
  # an entry for each population segment.
  .CheckLength(kwl, data.dt[, .N])

  # Simulate the audience size, i.e., the population making queries.
  data.dt[,
          audience := rbinom(length(pop), pop,
                             .MultiplyBySegment(audience.membership))]

  # Simulate the volume of search and the amount of spend.
  # Simulate the total number of queries.
  query.rate <- .ReadRepeatingVector(query.rate, current.time)
  qv.total <- rpois(1, query.rate * data.dt[, sum(audience)])
  # Simulate the number of matching queries.
  matching.qv.total <- rbinom(
      1, qv.total,  sum(data.dt[, audience / sum(audience)] * kwl))
  # Calculate the maximum number of clicks, based on spend cap.
  cpc.min <- .ReadRepeatingVector(cpc.min, current.time)
  cpc.max <- .ReadRepeatingVector(cpc.max, current.time)
  cpc <- min(max(cpc.min, bid), cpc.max)  # cost per click
  click.cap <- ifelse(is.infinite(spend.cap),
                      Inf,
                      as.integer(spend.cap / cpc))  # remove the fractional part
  # Calculate the average clickthrough rate over all segments.
  data.dt[, ctr := .MultiplyBySegment(ctr)]
  average.ctr <- data.dt[, sum(audience * ctr) / sum(audience)]
  # Calculate the number of impressions needed to reach the click.cap.
  if (is.infinite(click.cap)) {
    imp.cap <- Inf
  } else if (click.cap == 0) {
    imp.cap <- 0L
  } else {
    # The number of failures before the final click is negative binomial.
    # Suppress warnings generated by integer overflow.
    imp.cap <- click.cap +
        suppressWarnings(rnbinom(1, click.cap, average.ctr))
    # Set imp.cap to Inf when integer overflow
    if (is.na(imp.cap)) {
      imp.cap <- Inf
    }
  }
  # Calculate the available number of paid impressions, based on bid's effect on
  # share of voice.
  if (cpc.max == cpc.min) {
    sov <- 1
  } else {
    sov <- min(max((bid - cpc.min) / (cpc.max - cpc.min), 0), 1)
  }
  imp.avail <- rbinom(1, matching.qv.total, sov)
  # Calculate the total number of paid impressions.
  imp.total <- min(imp.avail, imp.cap)
  # Calculate the total number of paid clicks.
  if (is.infinite(click.cap) || is.infinite(imp.cap)) {  # uncapped.
    click.total <- rbinom(1, imp.avail, average.ctr)
  } else if (imp.avail < imp.cap) {  # capped, limited by inventory.
    click.total <- rhyper(1, click.cap, imp.cap - click.cap, imp.avail)
  } else {  # capped, limited by cap.
    click.total <- click.cap
  }
  # Conditional on the totals, simulate the number of queries, matching
  # queries, paid impressions, and paid clicks, by segment
  # Clicks.
  if (data.dt[, sum(audience * ctr)] == 0) {
    data.dt[, clicks := 0]
  } else {
    data.dt[,
            clicks := rmultinom( 1, click.total, audience * ctr)]
  }
  # Impressions.
  if (data.dt[, sum(audience * (1 - ctr))] == 0) {
    data.dt[, imps := clicks]
  } else {
    data.dt[,
            imps := clicks + rmultinom(
                1, imp.total - click.total, audience * (1 - ctr))]
  }
  # Matching queries.
  if (data.dt[, sum(audience * kwl)] == 0) {
    data.dt[, matching.query.volume := imps]
  } else {
    data.dt[,
            matching.query.volume := imps +
                rmultinom(1, matching.qv.total - imp.total,
                          audience * kwl)]
  }
  # Queries.
  if (data.dt[, sum(audience * (1 - kwl))] == 0) {
    data.dt[, query.volume := matching.query.volume]
  } else {
    data.dt[,
            query.volume := matching.query.volume +
                rmultinom(1, qv.total - matching.qv.total,
                          audience * (1 - kwl))]
  }
  # Calculate the spend from the paid clicks.
  data.dt[, spend := fn.env$cpc * clicks]

  # Calculate the absolute reach (# of unique individuals) for each
  # volume type:
  # Organic queries.
  n.qv <- .SimulateNotEmptyUrns(data.dt[, query.volume], data.dt[, audience])
  # Paid impressions.
  n.imp <- .SimulateNotEmptyUrns(data.dt[, imps], n.qv)
  # Paid clicks.
  n.click <- .SimulateNotEmptyUrns(data.dt[, clicks], n.imp)

  # The proportion of people migrating depends on the relative effectiveness
  # of organic only seach, paid impressions with no paid click, and paid clicks
  # at moving consumers.
  migrating.pop <-
      rbinom(length(n.qv), n.qv - n.imp, relative.effectiveness[1]) +
      rbinom(length(n.imp), n.imp - n.click, relative.effectiveness[2]) +
      rbinom(length(n.click), n.click, relative.effectiveness[3])
  # Simulate population migration.
  .MigrateMultiple(
      data.dt, migrating.pop,
      names(transition.matrices),
      transition.matrices)
  return(invisible(NULL))
}
