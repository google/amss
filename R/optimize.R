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

#' Optimize the media budgets in a specified budget period.
#'
#' Given a budget period and a set of constraints, find the budget setting and
#' the associated media spend that maximizes the profit (revenue minus cost of
#' production and advertising spend).
#'
#' See \code{DefaultSalesModule} for details on how the relationship between
#' revenue, profit, units sold, and advertising spend is specified.
#'
#' @param object an object of class \code{amss.sim}
#' @param budget.period numeric, the budget period to be optimized, the default
#'   being the most current budget period.
#' @param t.start integer, the first time interval over which the result
#'   (profit) of the budget settings should be calculated.
#' @param t.end integer, the last time interval over which the result (profit)
#'   of the budget settings should be calculated.
#' @param lower.bound numeric vector, the lower bound on the budget for each
#'   media channel.
#' @param upper.bound numeric vector, the upper bound on the budget for each
#'   media channel.
#' @param sum.lower.bound numeric, the lower bound on the total advertising
#'   spend.
#' @param sum.upper.bound numeric, the upper bound on the total advertising
#'   spend.
#' @param scaled.pop.size numeric, the population is scaled to this size in
#'   order to increase the accuracy of estimated expected profit.
#' @return \code{OptimizeBudget} returns a \code{list} with elemtnts
#'   \describe{
#'     \item{opt.spend}{the optimal spend in each media channel.}
#'     \item{opt.budget}{the optimal budget in the specified budget period.}
#'     \item{opt.profit}{the profit resulting from the optimal budget.}
#'     \item{orig.profit}{the profit in the original dataset.}
#'   }
#'
#' @note A module does not necessarily force the spend in a budget period to
#' match the budget. For example, in the paid search module, the budget is used
#' as the lever that leads to increasing/decreasing search spend.
#' Users should expect a monotonic relationship between budget and spend, but
#' no more. The budget is useful as a parameter in simulation and optimization,
#' as it is the lever moving advertiser-controlled settings in each media
#' channel. The spend, which may depend on other factors outside of the
#' advertiser's control, cannot be directly optimized; it is not a direct
#' input into the simulator. However, any budget settings can be mapped to a
#' corresponding media spend, and this is reported as the optimal spend. The
#' optimal spend is more meaningful than the budget as a reporting metric, and
#' is the key output of \code{OptimizeSpend}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Use the amss.sim object test.data from the testing suite.
#' # Find the optimal budget for the third budget period.
#' OptimizeSpend(test.data, budget.period = 3)
#' }


OptimizeSpend <- function(
    object,
    budget.period = max(GetBudgetIdx(object)),
    t.start = match(budget.period, GetBudgetIdx(object)),
    t.end = object$params$time.n,
    lower.bound = 0, upper.bound = Inf,
    sum.lower.bound = 0, sum.upper.bound = Inf,
    scaled.pop.size = 1e18) {

  # Check parameters.
  assertthat::assert_that(length(budget.period) == 1)
  assertthat::assert_that(inherits(object, "amss.sim"))
  budget.period.time <- which(GetBudgetIdx(object) == budget.period)
  if (t.start > min(budget.period.time)) {
    warning("Profits calculated over time interval starting after the end of ",
            "the budget period.")
  }
  if (t.end < max(budget.period.time)) {
    warning("Profits calculated over time interval ending before the end of ",
            "the budget period.")
  }


  # Get original budget.
  orig.budget <- GetBudget(object)
  n.media <- ncol(orig.budget)

  # Check constraints, and reduce dimensionality of the optimization if
  # possible.
  new.constraints <- ReduceDimension(n.media, lower.bound, upper.bound,
                                     sum.lower.bound, sum.upper.bound)

  # Case 1: search space is empty, return NULL.
  if (is.null(new.constraints)) {
    return(NULL)
  }

  n.dim <- length(new.constraints$lower.bound)
  if (n.dim == 0) {
    # Case 2: search space is a single point, return that point.
    opt.x <- new.constraints$decoder(numeric())
  } else {
    # Case 3: the optimization problem is non-trivial.
    # Reduce noise in objective function by scaling up the population size.
    orig.pop <- GetPopulation(object)
    mod.object <- data.table::copy(object)
    lapply(mod.object$data.full,
           function(dt) dt[, pop := AdjustPopulation(pop, scaled.pop.size)])
    scaled.pop.size <- mod.object$data.full[[1]][, sum(pop)]
    pop.multiplier <- scaled.pop.size / orig.pop
    mod.object$params$nat.mig.params$population <- scaled.pop.size

    # Objective function for the optimizer. Given a vector v from the search
    # space with reduced dimensionality specified in the new constraints,
    # objective finds the corresponding budget settings and calculates the
    # resulting expected profit. It reports the negative expected profit as the
    # target for minimization.
    objective <- function(v) {
      new.budget <- orig.budget
      new.budget[budget.period, ] <- new.constraints$decoder(v)
      profit <- GenerateDataUnderNewBudget(
          mod.object,
          new.budget = new.budget * pop.multiplier,
          response.metric = "profit / pop.multiplier",
          t.start = t.start, t.end = t.end)
      return(-sum(profit[t.start:t.end]))
    }

    # Optimize.
    # Interior point algorithms need a starting point in the interior of the
    # search space.
    orig.x <- orig.budget[budget.period, ]
    if (all(orig.x > lower.bound) && all(orig.x < upper.bound) &&
        sum(orig.x) > sum.lower.bound && sum(orig.x) < sum.upper.bound) {
      interior.v <- new.constraints$encoder(orig.x)
    } else {
      interior.v <- GetInterior(
              new.constraints$lower.bound, new.constraints$upper.bound,
              new.constraints$sum.lower.bound, new.constraints$sum.upper.bound)
    }
    # Methodology depends on dimensionality.
    if (n.dim == 1) {
      # Use Brent method for 1-D optimization.
      optim.v <- optim(
          par = interior.v,
          fn = objective,
          method = "Brent",
          lower = max(new.constraints$lower.bound,
                      new.constraints$sum.lower.bound),
          upper = min(new.constraints$upper.bound,
                      new.constraints$sum.upper.bound),
          control = list(reltol = 1e-3))
    } else {
      # Dimensionality is 2 or greater. Use Nelder Mead for multi-dimensional
      # constrained optimization.
      ui <- rbind(diag(n.dim), -diag(n.dim), 1, -1)
      ci <- c(new.constraints$lower.bound, -new.constraints$upper.bound,
              new.constraints$sum.lower.bound, -new.constraints$sum.upper.bound)
      optim.v <- constrOptim(
          theta = interior.v,
          f = objective,
          grad = NULL,
          ui = ui[ci != -Inf, ],
          ci = ci[ci != -Inf],
          outer.eps = 10)
    }
    opt.x <- new.constraints$decoder(optim.v$par)
  }

  # Reporting.
  opt.budget <- orig.budget
  opt.budget[budget.period, ] <- opt.x
  opt.data <- GenerateDataUnderNewBudget(
      mod.object,
      new.budget = opt.budget * pop.multiplier,
      t.start = t.start, t.end = t.end)
  var.names <- names(opt.data)
  spend.vars <- var.names[grepl("\\.spend$", var.names)]
  opt.spend <- apply(opt.data[budget.period.time, spend.vars, with = FALSE],
                     2, sum) / pop.multiplier
  opt.profit <- opt.data[t.start:t.end, sum(profit / pop.multiplier)]
  orig.profit <- object$data[t.start:t.end, sum(profit)]
  return(list(opt.spend = opt.spend,
              opt.budget = opt.x,
              opt.profit = opt.profit,
              orig.profit = orig.profit))
}

#' Find an interior point in a bounding box.
#'
#' The current optimization tool requires an interior point of the
#' set we are optimizing over. We assume that the constraints consist of a
#' bounding box (lower and upper bounds on each coordinate) and bounds on the
#' vector sum.
#'
#' This function finds an interior point of the set bounded in each coordinate
#' by lower bounds \code{lower.bound} and upper bounds \code{upper.bound}, and
#' with sum bounded between \code{sum.lower.bound} and \code{sum.upper.bound}.
#' It firsts modifies the constraints to be finite, and then finds the point x
#' along the line passing from lower.bound to upper.bound such that
#' sum(x) = (sum.lower.bound + sum.upper.bound) / 2.
#'
#' @param lower.bound: vector of lower bounds for each coordinate.
#' @param upper.bound: vector of upper bounds for each coordinate.
#' @param sum.lower.bound: numeric, lower bound on the sum of all coordinates.
#' @param sum.upper.bound: numeric, upper bound on the sum of all coordinates.
#'
#' @return \code{GetInterior} returns a vector representing an interior point in
#'   the set.
#'
#' @keywords internal

GetInterior <- function(lower.bound, upper.bound,
                        sum.lower.bound, sum.upper.bound) {

  # Check inputs to make sure an interior point exists.
  sum.lower.bound <- max(sum(lower.bound), sum.lower.bound)
  sum.upper.bound <- min(sum(upper.bound), sum.upper.bound)
  if (any(lower.bound >= upper.bound) || sum.lower.bound >= sum.upper.bound) {
    return(NULL)
  }

  # Find an achievable value for the vector sum of an interior point.
  if (is.infinite(sum.upper.bound)) {
    sum.upper.bound <- max(0, 2 * sum.lower.bound, sum.lower.bound / 2,
                           sum.lower.bound + 1)
  }
  if (is.infinite(sum.lower.bound)) {
    sum.lower.bound <- min(0, sum.upper.bound / 2, sum.upper.bound * 2,
                           sum.upper.bound - 1)
  }
  sum.center <- mean(c(sum.lower.bound, sum.upper.bound))

  # 1-dimensional case.
  n.dim <- length(lower.bound)
  if (n.dim == 1) {
    return(sum.center)
  }

  # Make the bounding box finite in such a way that it still
  #   (1) has non-empty interior.
  #   (2) the interior intersects with the hyperplane sum(x) = sum.center.
  upper.bound <- pmin(
      sum.upper.bound - sapply(1:n.dim, function(n) sum(lower.bound[-n])),
      upper.bound)
  upper.bound[upper.bound == Inf] <-
      pmax(2 * lower.bound[upper.bound == Inf],
           lower.bound[upper.bound == Inf] / 2,
           lower.bound[upper.bound == Inf] + 1, 0)
  lower.bound <- pmax(
      sum.lower.bound - sapply(1:n.dim, function(n) sum(upper.bound[-n])),
      lower.bound)

  # Find an interior point of bounding box. Specifically, a point along the
  # line segment from lower.bound to upper.bound that sums to sum.center.
  alpha <- (sum(upper.bound) - sum.center) /
      (sum(upper.bound) - sum(lower.bound))
  interior.point <- alpha * lower.bound + (1 - alpha) * upper.bound
  return(interior.point)
}

#' Reduce the dimensionality of the optimization
#'
#' Reduce the dimensionality of the optimization and find a function that
#' maps the new search space of the optimization to the original search space.
#'
#' The search space is assumed to be the intersection of a bounding box and an
#' upper/lower bound on the vector sum. If possible, the function finds an
#' equivalent lower-dimensional search space of the same type, and provides a
#' function mapping the new search space to the old search space.
#'
#' @param n.dim integer, original dimensionality of the optimization.
#' @param lower.bound numeric vector, the constraint specifying the lower bound
#'   in each dimension.
#' @param upper.bound numeric vector, the constraint specifying the upper bound
#'   in each dimension.
#' @param sum.lower.bound numeric, the lower bound on the vector sum.
#' @param sum.upper.bound numeric, the upper bound on the vector sum.
#'
#' @return \code{ReduceDimension} returns \code{NULL} if the constrained set
#'   is empty. Else, it returns a code{list} with elements
#'   \describe{
#'     \item{lower.bound}{the new lower bound.}
#'     \item{upper.bound}{the new upper bound.}
#'     \item{sum.lower.bound}{the new lower bound on the vector sum.}
#'     \item{sum.upper.bound}{the new upper bound on the vector sum.}
#'     \item{decoder}{function mapping values in the new search space to values
#'       in the original search space.}
#'     \item{encoder}{function mapping values in the original search space to
#'       values in the new search space.}
#'   }
#'
#' @keywords internal

ReduceDimension <- function(n.dim, lower.bound, upper.bound,
                            sum.lower.bound, sum.upper.bound) {

  # Check dimensionality of upper and lower bounds.
  lower.bound <- CheckLength(lower.bound, n.dim)
  upper.bound <- CheckLength(upper.bound, n.dim)

  # Parameter check: make sure the constraints describe a nonempty set.
  if (any(lower.bound > upper.bound) || sum.lower.bound > sum(upper.bound) ||
      sum.upper.bound < sum(lower.bound)) {
    warning("Optimization constraints describe empty set.")
    return(NULL)
  }
  # Also check infinite bounds.
  if (any(lower.bound == Inf) || any(upper.bound == -Inf) ||
      sum.lower.bound == Inf || sum.upper.bound == -Inf) {
    warning("Lower bounds cannot be Inf, upper bounds cannot be -Inf.")
    return(NULL)
  }

  # Reduce dimensionality of optimization by tracking which variables are
  # determined by the constraints.
  free.idx <- lower.bound != upper.bound

  # Remove equality conditions from bounding box.
  equalities <- lower.bound[!free.idx]
  sum.lower.bound <- sum.lower.bound - sum(equalities)
  sum.upper.bound <- sum.upper.bound - sum(equalities)
  lower.bound <- lower.bound[free.idx]
  upper.bound <- upper.bound[free.idx]

  # If there is an equality constraint on the sum, reduce the dimensionality.
  if (any(free.idx) && sum.lower.bound == sum.upper.bound) {
    equality.sum <- sum.lower.bound
    sum.upper.bound <- sum.upper.bound - lower.bound[1]
    sum.lower.bound <- sum.lower.bound - upper.bound[1]
    lower.bound <- lower.bound[-1]
    upper.bound <- upper.bound[-1]
  } else {
    equality.sum <- NULL
  }

  # Map the new search space to the original one.
  decoder <- function(v) {  # new to old
    x <- vector("numeric", n.dim)
    x[!free.idx] <- equalities
    if(!is.null(equality.sum)) {
      x[free.idx] <- c(equality.sum - sum(v), v)
    } else {
      x[free.idx] <- v
    }
    return(x)
  }
  encoder <- function(x) {  # old to new
    v <- x[free.idx]
    if (!is.null(equality.sum)) {
      return(v[-1])
    } else {
      return(v)
    }
  }

  return(list(lower.bound = lower.bound,
              upper.bound = upper.bound,
              sum.lower.bound = sum.lower.bound,
              sum.upper.bound = sum.upper.bound,
              decoder = decoder,
              encoder = encoder))
}
