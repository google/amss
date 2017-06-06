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
    "time.index", "pop", "brand.sales", "competitor.sales", "revenue",
    "profit", "total.spend"))

#' Model advertiser and competitor sales.
#'
#' Simulate consumer purchase behavior, and thus the advertiser's and its
#' competitors' sales.
#'
#' @param data.dt data.table with rows corresponding to population segments and
#'   columns corresponding to specific variables
#' @param price numeric vector of product price over time. If the vector is
#'   shorter than the number of timepoints, it is repeated as necessary.
#' @param mean.price numeric scaler, the mean of price over time. Defaults to
#'   zero.
#' @param advertiser.demand.intercept list of numeric vectors corresponding to
#'   each brand state (favorability, loyalty, and availability). The
#'   product of multiplicands corresponding to a particular segment with
#'   'purchase' activity state is the probability consumers in that
#'   segment will purchase the advertiser's product if the price is
#'   mean.price and there is no competition. Missing members of the list have no
#'   effect on the calculation.
#' @param advertiser.demand.slope list of numeric vectors corresponding to each
#'   brand state (favorability, loyalty, and availability). The product of
#'   multiplicands corresponding to a particular segment with 'purchase'
#'   activity state is the linear decrease in the probability consumers in
#'   that segment will purchase the advertiser's product when the price
#'   increases by 1, when there is no competition. Missing members of the
#'   list have no effect on the calculation.
#' @param competitor.demand.max list of numeric vectors corresponding to each
#'   brand state (favorability, loyalty, and availability). The product of
#'   multiplicands corresponding to a particular segment with 'purchase'
#'   activity state is the probability consumers in that segment will
#'   purchase a competitor's product when advertiser's product is too
#'   expensive to be a feasible choice. Missing members of the list have
#'   no effect on the calculation.
#' @param competitor.demand.replacement list of numeric vectors corresponding
#'   to each brand state (favorability, loyalty, and availability). The
#'   product of multiplicands corresponding to a particular segment
#'   specifies the degree to which advertiser and competitor sales are
#'   replacements for each other. At 1, competitor sales are unaffected by
#'   advertiser pricing, and competitor sales replace advertiser sales to
#'   the greatest degree possible. At 0, advertiser sales are unaffected
#'   by the presence of the competitor, and advertiser sales replace
#'   competitor sales to the greatest degree possible. Thus, a reasonble
#'   interpretation of consumer loyalty might set this parameter to
#'   \code{list(loyalty = c(0.5, 0.1, 0.9)}. Missing members of the list
#'   have no effect on the calculation.
#' @param purchase.quantity.intercept numeric, at least 1. Represents the
#'   average number of units bought by each consumer purchasing from the
#'   advertiser's brand, if price is mean.price.
#' @param purchase.quantity.slope numeric, generally >= 0. Represents the
#'   decrease in the average purchase quantity per consumer purchasing
#'   from the advertiser's brand given a unit increase in price. Missing
#'   members of the list have no effect on the calculation.
#' @param purchase.quantity.competitor average number of units bought by
#'   consumers purchasing a comeptitor's product. Must be at the least the
#'   default value of 1.
#' @param unit.cost numeric greater than 0, cost of goods sold, for one unit of
#'   the advertiser's product.
#' @param advertiser.transitions list of transition matrices for each brand
#'   state, specifying post-purchase changes in consumer mindset for those
#'   who purchased the advertiser's brand. A named list with members
#'   'favorability', 'loyalty', and 'availability' is expected. Any
#'   missing members will have no effect. The default value, \code{list()}
#'   results in no post-purchase migration.
#' @param competitor.transitions list of transition matrices for each brand
#'   state, specifying post-purchase changes in consumer mindset for those
#'   who purchased a competitor's brand. A named list with members
#'   'favorability', 'loyalty', and 'availability' is expected. Any
#'   missing members will have no effect. The default value, \code{list()}
#'   results in no post-purchase migration.
#' @return \code{invisible(NULL)}. \code{data.dt} updated by reference.
#' @export

DefaultSalesModule <- function(
    data.dt, price, mean.price = 0,
    advertiser.demand.intercept = list(),
    advertiser.demand.slope = list(
        favorability = rep(0, length(kFavorabilityStates))),
    competitor.demand.max = list(loyalty = c(1, 0, 1)),
    competitor.demand.replacement = list(loyalty = c(0.5, 0, 1)),
    purchase.quantity.intercept = 1,
    purchase.quantity.slope = 0,
    purchase.quantity.competitor = 1,
    unit.cost = 0,
    advertiser.transitions = list(),
    competitor.transitions = list()) {

  # Setup.
  fn.env <- environment()
  current.time <- data.dt[1, time.index]
  if (!("activity" %in% names(advertiser.demand.intercept))) {
    advertiser.demand.intercept <- c(advertiser.demand.intercept,
                                     list(activity = c(0, 0, 1)))
  }
  if (!("activity" %in% names(advertiser.demand.slope))) {
    advertiser.demand.slope <- c(advertiser.demand.slope,
                                 list(activity = c(0, 0, 1)))
  }
  if (!("activity" %in% names(competitor.demand.max))) {
    competitor.demand.max <- c(competitor.demand.max,
                               list(activity = c(0, 0, 1)))
  }

  # Check parameters.
  if (current.time == 1) {
    assertthat::assert_that(is.numeric(price), all(price >= 0))
    CheckSalesActivity(advertiser.demand.intercept)
    CheckSalesActivity(advertiser.demand.slope)
    CheckSalesActivity(competitor.demand.max)
    assertthat::assert_that(is.numeric(purchase.quantity.intercept))
    assertthat::assert_that(is.numeric(purchase.quantity.slope))
    if (any(purchase.quantity.slope < 0)) {
      warning("Generally, the user should specify positive",
              " <purchase.quantity.slope> for a negative price vs. quantity",
              " purchased by each purchaser relationship.")
    }
    CheckListNames(advertiser.transitions, colnames(kBrandStates))
    CheckListNames(competitor.transitions, colnames(kBrandStates))
  }

  # Get product price.
  current.price <- price[(current.time - 1) %% length(price) + 1]

  # Calculate the probability of purchasing the advertiser's brand, given no
  # competition.
  advertiser.demand.intercept <- MultiplyBySegment(advertiser.demand.intercept)
  advertiser.demand.slope <- MultiplyBySegment(advertiser.demand.slope)
  if (current.time == 1 && any(advertiser.demand.slope < 0)) {
    warning("Generally, the user should specify positive",
            " <advertiser.demand.slope> for a negative relationship",
            " between price and the number of purchasers.")
  }
  nocompetition.advertiser.demand <- pmin(
      1,
      pmax(0,
           advertiser.demand.intercept -
           advertiser.demand.slope * (current.price - mean.price)))

  # Add competition, and calculate final probabilities of consumers
  # purchasing from the advertiser vs. competitor brands.
  competitor.demand.max <-
      pmax(0, pmin(1, MultiplyBySegment(competitor.demand.max)))
  competitor.demand.replacement <-
      pmax(0, pmin(1, MultiplyBySegment(competitor.demand.replacement)))
  competitor.demand <- pmax(
      0,
      competitor.demand.max -
          (1 - competitor.demand.replacement) * nocompetition.advertiser.demand)
  advertiser.demand <- pmax(  # Subtract competitor demand from total demand.
      pmax(nocompetition.advertiser.demand,
           competitor.demand.max) - competitor.demand,
      0)  # Avoid the numerical error of 'negative' 0.

  # Simulate the number of people making advertiser, competitor purchases.
  n.purchasing <- matrix(0L, nrow(data.dt), 2)
  for (iter.seg in 1:nrow(data.dt)) {
    n.purchasing[iter.seg, ] <-
        RMultinom(1, data.dt[iter.seg, pop],
                  c(advertiser.demand[iter.seg], competitor.demand[iter.seg],
                    1 - advertiser.demand[iter.seg] -
                    competitor.demand[iter.seg]))[1:2]

  }

  # Calculate the average number of units sold per purchaser.
  advertiser.units.per.purchaser <- max(
      1,
      purchase.quantity.intercept -
      purchase.quantity.slope * (current.price - mean.price))
  competitor.units.per.purchaser <- max(1, purchase.quantity.competitor)
  # Simulate the total number of sales.
  data.dt[,
          brand.sales := fn.env$n.purchasing[, 1] +
              RPois(nrow(n.purchasing),
                    n.purchasing[, 1] * (advertiser.units.per.purchaser - 1))]
  data.dt[,
          competitor.sales := fn.env$n.purchasing[, 2] +
              RPois(nrow(n.purchasing),
                    n.purchasing[, 2] * (competitor.units.per.purchaser - 1))]

  # Calculate the revenue and profit.
  data.dt[, revenue := brand.sales * fn.env$current.price]
  data.dt[, profit := revenue - unit.cost * brand.sales - total.spend]

  # Simulate post-purchase population migration.
  # Simulate changes in people who bought the advertiser's brand.
  advertiser.transitions <- c(  # All purchasers satiate.
      advertiser.transitions,
      list(satiation = matrix(c(1, 1, 0, 0), 2)))
  MigrateMultiple(data.dt, n.purchasing[, 1],
                  names(advertiser.transitions), advertiser.transitions)
  # Simulate changes in people who bought the competitor's brand.
  competitor.transitions <- c(  # All purchasers satiate.
      competitor.transitions,
      list(satiation = matrix(c(1, 1, 0, 0), 2)))
  MigrateMultiple(data.dt, n.purchasing[, 2],
                  names(competitor.transitions), competitor.transitions)
}

#' Warn users of possibility of consumers outside the 'purchase' state
#' purchasing.
#'
#' Checks parameters in the sales module to make sure that the probability of
#' consumers who are not in the 'purchase' activity state is 0.
#'
#' @param x the parameters being checked
#' @return \code{NULL}. If the parameter specification breaks enforcement of
#'   only consumers who have attained the 'purchase' state being able to make a
#'   purchase, the function signals a warning.
#' @keywords internal

CheckSalesActivity <- function(x) {

  # Check condition, and output warning if failed.
  if (any(x$activity[1:2] != 0)) {
    warning("It is recommended to limit purchasing to individuals in",
            " the 'purchase' activity state. The current specification of",
            " 'advertiser.demand.intercept$activity' overwrites the code",
            " enforcing this. Consider removing this member of the list.")
  }
  return(NULL)
}
