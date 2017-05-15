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

utils::globalVariables(c("pop", "pop.in", "pop.out"))

#' Apply transition matrix to vector of population counts by state.
#'
#' Applies transition matrix to vector of population counts by state.
#' Option to select transition matrix that matches length of vector when given
#' multiple transition matrices.
#'
#' @param vector.counts vector of counts in each state
#' @param transition.matrices transition matrix, or list of transition
#'   matrices. When multiple transition matrices are supplied, the first
#'   transition matrix whose dimensionality matches the number of states
#'   in vector.counts is used.
#' @return number of individuals in each state after migration
#' @keywords internal

ApplyTransitionMatrix <- function(vector.counts, transition.matrices) {

  # Given a transition matrix, simulate the migration process.
  if (!is.list(transition.matrices)) {
    # Check inputs.
    if (length(vector.counts) != nrow(transition.matrices)) {
      stop("Dimension mismatch between vector and transition matrix.")
    }
    if (length(vector.counts) == 1 ||
        identical(transition.matrices, diag(nrow(transition.matrices)))) {
      return(vector.counts)
    }

    # Calculate migration behavior of the population originating from each
    # segment
    migrated.pop <- integer(length(vector.counts))
    for (iter in 1:length(vector.counts)) {
      migrated.pop <- migrated.pop +
          drop(RMultinom(1, vector.counts[iter], transition.matrices[iter, ]))
    }

    # Return new population segmentation.
    return(migrated.pop)
  }

  # If multiple transition matrices are provided, choose the correct one
  # based on the dimensionality of the matrix and the length of the vector of
  # counts.
  num.states <- sapply(transition.matrices, nrow)
  transition.matrix <-
      transition.matrices[[match(length(vector.counts), num.states)]]
  return(ApplyTransitionMatrix(vector.counts, transition.matrix))
}

#' Simulate migration in a single dimension of population segmentation.
#'
#' Simulate migration of indivduals between different states in single
#' dimension of population segmentation, such as "brand favorability."
#'
#' @param data.dt data.table with rows representing population segments and
#'   columns representing specific variables.
#' @param migrating.pop.size migrating population size
#' @param migration.dim name dimension of migration, must be a column of
#'   kAllStates.
#' @param transition.matrix transition matrix specifying probabilities of
#'   migration between states.
#' @return \code{invisible(NULL)}. \code{data.dt} is updated by reference.
#' @keywords internal

MigrateMarginal <- function(data.dt, migrating.pop.size,
                            migration.dim, transition.matrix) {

  # Check input.
  func.env <- environment()
  data.dt[, pop.out := func.env$migrating.pop.size]
  if (!(migration.dim %in% names(kAllStates))) {
    stop("Invalid dimension of migration.")
  }

  if (migration.dim %in% c("market", "satiation")) {
    # For market and satiation state, make sure segments that are
    # not inactive also transition to the 'out.market' or 'satiated' state,
    # simultaneously becoming inactive.

    # The transition matrix for these cases is specified by column, i.e.,
    # destination state
    expanded.transition.matrix <- matrix(
        # (out.market XOR unsatiated), inactive
        c(transition.matrix[, 1], transition.matrix[2, 1],
          transition.matrix[2, 1],
          # in.market, unsatiated, inactive
          transition.matrix[, 2], 0, 0,
          # in.market, unsatiated, exploratory
          0, 0, transition.matrix[2, 2], 0,
          # in.market, unsatiated, purchase
          0, 0, 0, transition.matrix[2, 2]), 4)
    transition.matrices <- list(transition.matrix, expanded.transition.matrix)
    # Activity state (dimension 3) may change.
    changing.dimensions <- c(migration.dim, "activity")
  } else if (migration.dim %in% c("activity", "favorability", "availability")) {
    transition.matrices <- list(transition.matrix, matrix(1))
    changing.dimensions <- migration.dim
  } else {  # Special case for loyalty transitions.
    # Favorable consumers transition freely.
    # Not favorable consumers cannot become loyal. Thus, in the restricted
    # transition matrix, the probability of becoming loyal is reassigned to
    # becoming a switcher.
    restricted.transition.matrix <-
        # Take the original transition probabilities of becoming a switcher or
        # competitor-loyal.
        transition.matrix[c(1, 3), c(1, 3)] +
        # Instead of becoming loyal, become a switcher
        matrix(c(transition.matrix[c(1, 3), 2], 0, 0), 2)
    transition.matrices <- list(transition.matrix, restricted.transition.matrix)
    changing.dimensions <- migration.dim
  }
  data.dt[,
          pop.in := ApplyTransitionMatrix(pop.out, transition.matrices),
          by = eval(setdiff(key(data.dt), changing.dimensions))]
  data.dt[, pop := pop - pop.out + pop.in]
  return(invisible(NULL))
}

#' Simulate migration in multiple dimensions of population segmentation.
#'
#' Perform successive migrations of consumers between population segments, with
#' each migration focusing on changes in a particular dimension of population
#' segmentation.
#'
#' @param data.dt data.table with rows representing population segments and
#'   columns representing specific variables.
#' @param migrating.pop.size migrating population size
#' @param migration.dims a character vector of dimensions of migration, by
#'   name.
#' @param transition.matrices a list of transition matrices for each dimension.
#' @return \code{invisible(NULL)}. \code{data.dt} is updated by reference.
#' @keywords internal

MigrateMultiple <- function(data.dt, migrating.pop.size,
                            migration.dims = character(),
                            transition.matrices = list()) {

  # Check input.
  stopifnot(length(transition.matrices) == length(migration.dims))

  # Perform each migration in sequence.
  if (length(migration.dims) > 0) {
    for (iter.dim in 1:length(migration.dims)) {
      MigrateMarginal(data.dt, migrating.pop.size, migration.dims[iter.dim],
                      transition.matrices[[iter.dim]])
      migrating.pop.size <- data.dt[, pop.in]
    }
  }
  return(invisible(NULL))
}
