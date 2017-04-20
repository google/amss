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

#' @import data.table
NULL

#' Constant defining the market states.
#'
#' \code{kMarketStates} is a character vector of all valid market states a
#' consumer may take. Consumers who have no interest in the category should be
#' considered "out-of-market." Consumers' market states may vary over time
#' due to seasonal changes in demand, but generally should not be changed by
#' marketing interventions.
#' @export

kMarketStates <- c("out.market", "in.market")

#' Constant defining the satiation states.
#'
#' \code{kSatiationStates} is a character vector of all valid satiation states a
#' consumer may take. Satiation tracks whether a consumer's demand for the
#' product category is temporarily satisfied by a recent purchase.
#' @export

kSatiationStates <- c("satiated", "unsatiated")

#' Constant defining the activity states.
#'
#' \code{kActivityStates} is a character vector of all valid activity states a
#' consumer may take. Activity state tracks what category-related activities
#' the consumer is engaged in.
#' @export

kActivityStates <- c("inactive", "exploration", "purchase")

#' Constant defining the brand favorability states.
#'
#' \code{kFavorabilityStates} is a character vector of all valid brand
#' favorability states a consumer may take. Brand awareness is also tracked
#' through this dimension; consumers are either "unaware" of the advertiser's
#' brand, or have a brand favorability ranging from "negative" to "favorable."
#' @export

kFavorabilityStates <- c("unaware", "negative", "neutral",
                         "somewhat favorable", "favorable")

#' Constant defining the brand loyalty states.
#'
#' \code{kLoyaltyStates} is a character vector of all valid brand loyalty states
#' a consumer may take.
#' @export

kLoyaltyStates <- c("switcher", "loyal", "competitor-loyal")

#' Constant defining the brand availability states.
#'
#' \code{kAvailabilityStates} is a character vector of all valid brand
#' availability states a consumer may take. Brand availability may refer to
#' the physical availability of the advertiser's product to a particular
#' consumer, or to the mental availability, i.e., the convenience.
#' @export

kAvailabilityStates <- c("low", "average", "high")

#' Generate the list of category states.
#'
#' Category states are a combination of market, satation, and activity state.
#' This functions produces a list all valid combinations of these three
#' dimensions of population segmentation.
#'
#' @return \code{data.frame} with columns \code{market}, \code{satiation}, and
#'   \code{activity}, listing all valid combinations of market, satiation,
#'   and activity states

.GetCategoryStates <- function() {

  # Find all combinations of states.
  category.states <- expand.grid(
      activity = factor(kActivityStates, kActivityStates),
      satiation = factor(kSatiationStates, kSatiationStates),
      market = factor(kMarketStates, kMarketStates))

  # Remove invalid combinations.
  return(category.states[category.states$activity == "inactive" |
                         (category.states$market == "in.market" &
                          category.states$satiation == "unsatiated"), ])
}

#' Constant defining the category states.
#'
#' A \code{data.frame} of all valid combinations of market state, satiation
#' state, and activity state, given that only consumers who are both
#' \code{in.market} and \code{unsatiated} can have activity states other than
#' \code{inactive}. Category state summarizes the consumer's relationship with
#' the category (as opposed to brand-specific relationships).
#' @export

kCategoryStates <- .GetCategoryStates()

#' Generate the list of brand states.
#'
#' Brand states are a combination of brand favorability, brand loyalty, and
#' brand availability. This functions produces a list all valid combinations
#' of these three dimensions of population segmentation.
#'
#' @return \code{data.frame} with column \code{brand} containing all valid
#'   values for brand state.

.GetBrandStates <- function() {

  # Find all combinations of states.
  brand.states <- expand.grid(
      availability = factor(kAvailabilityStates, kAvailabilityStates),
      loyalty = factor(kLoyaltyStates, kLoyaltyStates),
      favorability = factor(kFavorabilityStates, kFavorabilityStates))

  # Remove invalid combinations.
  return(brand.states[!(brand.states$favorability != "favorable" &
                        brand.states$loyalty == "loyal"), ])
}

#' Constant defining the brand states.
#'
#' A \code{data.table} of all valid combinations of brand favorability state,
#' brand loyalty state, and brand availability state, given that only consumers
#' with a \code{favorable} opinion of the brand can be \code{loyal}. Brand
#' state summarizes the consumer's relationship with the advertiser's brand.
#' @export

kBrandStates <- .GetBrandStates()

#' Constant defining all consumer states.
#'
#' A \code{data.table} of all valid consumer states. It is the cross product of
#' category and brand states. Every consumer is assigned to one of these 198
#' states.
#' @export

kAllStates <- data.table::setDT(merge(kBrandStates, kCategoryStates))
data.table::setkey(kAllStates, market, satiation, activity,
                   favorability, loyalty, availability)
data.table::setcolorder(kAllStates, data.table::key(kAllStates))
