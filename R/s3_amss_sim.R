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
#' Creates objects of class \code{amss.sim}.
#'
#' @param data a \code{data.table} containing the observed data.
#' @param data.full the full data, as list of \code{data.tables} with rows
#'   corresponding to each segment and columns corresponding to specific
#'   variables. Each \code{data.table} corresponds to a single time interval.
#' @param params parameters used in specifying the simulation settings.
#' @return An object of class \code{amss.sim}, is a list with the following
#'   elements:
#'   \describe{
#'     \item{data}{the observed data.}
#'     \item{data.full}{the full dataset, as a list of data.tables. Each
#'       \code{data.table} contains the data at the end of a time interval, by
#'       by population segment (row) and variable (column).}
#'     \item{params}{the parameters used to generate the data.}
#'   }
#' @keywords internal

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
#' @keywords internal

GetBudget <- function(object) {

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
#' @keywords internal

GetPopulation <- function(object) {

  # Check input.
  assertthat::assert_that(inherits(object, "amss.sim"))

  # Return population size, as read from the parameters.
  return(object$params$nat.mig.params$population)
}

#' Get the budget period assigned to each time interval.
#'
#' Read the budget indices from an \code{amss.sim} object.
#'
#' The budget indices specify which time intervals belong to the same budget
#' period.

GetBudgetIdx <- function(object) {

  return(object$params$media.params[[1]]$budget.index)
}
