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

#' Create AMSS simulation objects.
#'
#' Creates objects of class \code{amss.sim}, containing full information
#' about simulated data.
#'
#' @param data observed data
#' @param data.full full data, as list of data.tables with rows corresponding
#'   to each segment and columns corresponding to specific variables. each
#'   data.table corresponds to a single time point.
#' @param params parameters used to generate the simulation
#' @return object of class \code{amss.sim}, containing the observed data, the
#'   full dataset, and a prediction function.
#' @export

amss.sim <- function(data = NULL, data.full = NULL, params = NULL) {

  # Check input type is correct.
  assertthat::assert_that(is.data.table(data))
  assertthat::assert_that(is.list(data.full))
  assertthat::assert_that(all(sapply(data.full, is.data.table)))
  assertthat::assert_that(is.list(params))

  # Assemble object.
  sim.object <- list(data = data,
                     data.full = data.full,
                     params = params)

  # Update object class.
  class(sim.object) <- "amss.sim"
  return(sim.object)
}

#' Get budget information from a simulation object.
#'
#' Retrieves information on media budgets from an object of class
#' \code{amss.sim}.
#'
#' @param object object of class amss.sim
#' @return matrix of budgets by budget period (row) and media name (column)

.GetBudget <- function(object) {

  # Check input.
  assertthat::assert_that(inherits(object, "amss.sim"))

  # Return NULL if there are no media budgets.
  if (length(object$params$media.params) == 0) {
    return(NULL)
  }

  # Get budget values.
  budget.matrix <- sapply(object$params$media.params, function(x) x$budget)

  # Label each column with the media name.
  colnames(budget.matrix) <- object$params$media.names
  return(budget.matrix)
}

#' Get population size from a simulation object.
#'
#' Retrieves population size from an object of class \code{amss.sim}.
#'
#' @param object object of class amss.sim
#' @return integer population size

.GetPopulation <- function(object) {

  # Check input.
  assertthat::assert_that(inherits(object, "amss.sim"))

  # Return population size, as read from the parameters.
  return(as.integer(object$params$nat.mig.params$population))
}
