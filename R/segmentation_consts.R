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

#' Population segmentation constants
#'
#' AMSS segments the population segments the population into groups based
#' on each consumer's current mindset with regards to the category and the
#' advertiser's brand. Aggregate changes in the population are tracked through
#' the size of, i.e., the number of individuals belonging to, each population
#' segment.
#'
#' The consumer minset is defined along six dimensions. The first three specify
#' the consumer's relationship with the category:
#' \describe{
#'   \item{Market state}{describes whether the consumer should be
#'     considered part of the market for this category. Consumers with no
#'     interest are \code{out.of.market}; the rest are \code{in.market}. A
#'     consumer's market state may vary over time due to seasonal changes in
#'     consumer demand, but generally should not be affected by marketing
#'     interventions.}
#'   \item{Satiation state}{tracks whether a consumer's demand for the
#'     product category is temporarily satisfied by a recent purchase.}
#'   \item{Activity state}{tracks the consumer's progress along the path to
#'     purchase. Consumers may be in the \code{inactive}, \code{exploratory}, or
#'     \code{purchase} state. Consumers in different activity states will have
#'     different behaviors. For example, by default consumers outside the
#'     \code{purchase} state will never make a purchase. Activity state also
#'     affects media consumption; for example, individuals who are not
#'     \code{inactive} are generally more likely to make generic or branded
#'     search queries.}
#' }
#' The last three dimensions describe the consumer's relationships with the
#' advertiser's brand.
#' \describe{
#'   \item{Brand favorability state}{specifies a consumer's awareness of and
#'     opinion of the advertiser's brand. Consumers are either \code{unaware},
#'     or are aware and have an opinion of the brand ranging from
#'     \code{negative} to \code{favorable}, with intermediate favorabilitiy
#'     levels \code{neutral} and \code{somewhat favorable}.}
#'   \item{Brand loyalty state}{specifies a consumer's loyalty status. A
#'     consumer may be a \code{switcher}, in which case he or she has no brand
#'     loyalty. Otherwise the consumer is either \code{loyal}, i.e., loyal to
#'     the advertiser's brand, or \code{competitor.loyal}.}
#'   \item{Brand availability state}{refers to whether the advertiser's product
#'     is easily available to a particular consumer. For example, if the
#'     advertiser's distribution efforts only cover seventy percent of the
#'     population, then the thirty percent of the population not covered would
#'     be in the \code{low} brand availability state. The other options are
#'     \code{average} and \code{high} brand availability. Availability can
#'     refer to physical availability, i.e. the presence of the advertiser's
#'     product on store shelves. It could also refer to the mental availability
#'     (convenience) of the advertiser's brand. Thus brand availability can
#'     be affected by, say search ads that make the advertiser's brand the most
#'     prominent on the search results page, or by having the advertiser's
#'     product at eye-level in a store shelf.}
#' }
#'
#' The constants \code{kMarketStates}, \code{kSatiationStates},
#' \code{kActivityStates}, \code{kFavorabilityStates}, \code{kLoyaltyStates},
#' and \code{kAvailabilityStates} list the the possible states a consumer may
#' take in each dimension as character vectors.
#'
#' A consumer's mindset is summarized by the combination of states they take
#' in each dimension. There are certain restrictions on which combinations of
#' consumer states are possible. For example, only consumers who are both
#' \code{in.market} and \code{unsatiated} can leave the \code{inactive} activity
#' state. The \code{data.frame} \code{kCategoryStates} describes all valid
#' combinations of market state, satiation state, and activity state, and thus
#' lists all possible consumer mindsets with respect to the category in general.
#' The \code{data.frame} \code{kBrandStates} describes all valid combinations of
#' brand favorability, loyalty, and availability, given that only consumers
#' with a \code{favorable} opinion of the brand can be \code{loyal}. Thus,
#' \code{kBrandStates} lists all possible consumer mindsets with regards to the
#' advertiser's brand.
#'
#' A \code{data.table} of all valid consumer states is provided as
#' \code{kAllStates}. It is the cross product of all category and brand states.
#' Every consumer is assigned to one of these 198 states.
#'
#' @format An object of class \code{character} (all possible states in a single
#'   dimension) or \code{data.table} (each row specifying a valid combination of
#'   states in different dimensions).
#' @name population segmentation
#' @aliases kMarketStates kSatiationStates kActivityStates kFavorabilityStates
#'   kLoyaltyStates kAvailabilityStates kCategoryStates kBrandStates kAllStates
NULL

#' Vector of all market states.
#'
#' @rdname population segmentation
#' @export

kMarketStates <- c("out.market", "in.market")

#' Vector of all satiation states.
#'
#' @rdname population segmentation
#' @export

kSatiationStates <- c("satiated", "unsatiated")

#' Vector of all activity states.
#'
#' @rdname population segmentation
#' @export

kActivityStates <- c("inactive", "exploration", "purchase")

#' Vector of all brand favorability states.
#'
#' @rdname population segmentation
#' @export

kFavorabilityStates <- c("unaware", "negative", "neutral",
                         "somewhat favorable", "favorable")

#' Vector of all brand loyalty states.
#'
#' @rdname population segmentation
#' @export

kLoyaltyStates <- c("switcher", "loyal", "competitor-loyal")

#' Vector of all brand availability states.
#'
#' @rdname population segmentation
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
#' @keywords internal

GetCategoryStates <- function() {

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

#' Vector of all category states.
#'
#' @rdname population segmentation
#' @export

kCategoryStates <- setDT(GetCategoryStates())

#' Generate the list of brand states.
#'
#' Brand states are a combination of brand favorability, brand loyalty, and
#' brand availability. This functions produces a list all valid combinations
#' of these three dimensions of population segmentation.
#'
#' @return \code{data.frame} with column \code{brand} containing all valid
#'   values for brand state.
#' @keywords internal

GetBrandStates <- function() {

  # Find all combinations of states.
  brand.states <- expand.grid(
      availability = factor(kAvailabilityStates, kAvailabilityStates),
      loyalty = factor(kLoyaltyStates, kLoyaltyStates),
      favorability = factor(kFavorabilityStates, kFavorabilityStates))

  # Remove invalid combinations.
  return(brand.states[!(brand.states$favorability != "favorable" &
                        brand.states$loyalty == "loyal"), ])
}

#' Vector of all brand states.
#'
#' @rdname population segmentation
#' @export

kBrandStates <- setDT(GetBrandStates())

#' Vector of all consumer states.
#'
#' @rdname population segmentation
#' @export

kAllStates <- data.table::setDT(merge.data.frame(kBrandStates, kCategoryStates))
data.table::setkey(kAllStates, market, satiation, activity,
                   favorability, loyalty, availability)
data.table::setcolorder(kAllStates, data.table::key(kAllStates))
