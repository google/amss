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

utils::globalVariables(c("pop", "total.spend"))

#' Generate simulation objects under the AMSS framework.
#'
#' Produces an \code{amss.sim} object that contains the simulated data and can
#' be used to derive ground truth about the scenario.
#'
#' Objects of class \code{amss.sim} contain the full output from running a
#' simulation scenario. This output includes the observed data, the complete
#' dataset generated during the simulation process, and the parameters passed
#' to the simulation function in order to generate the simulated data. The
#' observed data is meant to be used by modelers. The complete dataset can be
#' useful for users who want a more complete understanding of the forces
#' operating in a simulation scenario. The parameter list is essential for
#' generating future datasets based on the same, or slightly modified,
#' simulation settings in order to obtain ground truth about the simulation
#' scenario.
#'
#' @param time.n number of timepoints.
#' @param nat.mig.module specifications of class \code{seq.specs}, to be used
#'   to create the non-actionable drivers module and then generate its
#'   variables
#' @param nat.mig.params any parameter values to pass to \code{nat.mig.module}
#' @param media.names character vector of unique names of all media modules.
#'   ex: c("tv", "search")
#' @param media.modules list of functions that simulate the behavior of each
#'   marketing intervention.
#' @param media.params list of parameter value lists for each media module.
#' @param sales.module function that models the sales in the category.
#' @param sales.params list of any parameter values to pass to the sales module
#' @param ping the spacing between time points at which to print a message
#'   updating the user on simulation progress.
#' @param names.agg.const character vector of names of variables to surface in
#'   the observed data, aggregated using the first entry, since they are
#'   constant over the hidden states. By default, if not specified,
#'   \code{SurfaceData()} will pick up variables with names containing
#'   "price" and/or "budget", and the "pop.total" variable.
#' @param names.agg.sum character vector of names of variables to surface in
#'   the observed data, aggregated using the function \code{sum()}. By
#'   default, \code{SurfaceData()} will pick up variables with names
#'   matching "revenue", "profit", "sales", "volume", and/or "spend".
#' @return an object of class \code{amss.sim}, containing
#'   \describe{
#'     \item{data}{the observed data.}
#'     \item{data.full}{the full dataset, as a list of data.tables. Each
#'       \code{data.table} contains the data at the end of a time interval, by
#'       by population segment (row) and variable (column).}
#'     \item{params}{the parameters used to generate the data.}
#'   }
#' @export

SimulateAMSS <- function(
    time.n,
    nat.mig.module = `DefaultNatMigModule`,
    nat.mig.params = list(),
    media.names = character(),
    media.modules = rep(list(`DefaultTraditionalMediaModule`),
                        length(media.names)),
    media.params = rep(list(list()), length(media.names)),
    sales.module = `DefaultSalesModule`,
    sales.params = list(),
    ping = max(10, floor(time.n / 10)),
    names.agg.const = NULL,
    names.agg.sum = NULL) {

  # Check inputs.
  assertthat::assert_that(time.n >= 1)
  # Different media channels must have different names.
  assertthat::assert_that(!anyDuplicated(media.names))

  # Save the parameter list.
  params <- as.list(match.call())[-1]
  default.params <- as.list(formals(`SimulateAMSS`))
  default.params[names(params)] <- NULL
  params <- c(params, default.params)
  eval.env <- environment()
  params <- lapply(params, function(x) eval(x, eval.env))

  # Simulate data.
  data.full <- do.call(
      SimulateData,
      c(list(starting.dts = list()), params)[formalArgs(`SimulateData`)])

  # Return an object of class amss.sim.
  return(amss.sim(
      data = SurfaceData(data.full, names.agg.const, names.agg.sum),
      data.full = data.full,
      params = params))
}

#' Simulate data using the AMSS framework.
#'
#' Data generating function for AMSS. Responsible for sequentially simulating
#' the value of each variable at each time point by population segment, given
#' the simulation settings.
#'
#' @param starting.dts previously generated data that we can start with. data
#'   will only be generated for later timepoints
#' @param time.n total number of timepoints, including the timepoints already
#'   existing in starting.dts.
#' @param nat.mig.module function used to simulate effects of natural migration
#'   on population segmentation.
#' @param nat.mig.params list of any parameter values to pass to
#'   \code{nat.mig.module}
#' @param media.names character vector of unique names of all media modules.
#'   ex: c("tv", "search")
#' @param media.modules list of functions that simulate the behavior of each
#'   marketing intervention.
#' @param media.params list of parameter value lists for each media module.
#' @param sales.module function that generate sales variables.
#' @param sales.params list of any parameter values to pass to the sales module
#' @param ping the spacing between time points at which to print a message
#'   updating the user on simulation progress.
#' @return a list of data sets, one per timepoint. Each is a data.table with
#'   rows corresponding to population segments and columns corresponding
#'   to specific variables.
#' @keywords internal

SimulateData <- function(
    starting.dts = list(),
    time.n,
    nat.mig.module, nat.mig.params,
    media.names, media.modules, media.params,
    sales.module, sales.params,
    ping) {

  # Initialize data with the starting.dts provided.
  all.dt <- data.table::copy(starting.dts)

  # Handle the special case: no new data to generate.
  if (time.n <= length(all.dt)) {
    return(all.dt[1:time.n])
  }

  # Generate data for each time interval sequentially.
  # Initialize curr.dt variable to store the most recent values for each
  # variable.
  t.start <- length(all.dt) + 1L
  if (t.start == 1) {
    curr.dt <- InitStateData(time.index = 0)
  } else {
    curr.dt <- data.table::copy(all.dt[[t.start - 1]])
  }
  # Iteratively simulate data for each time interval.
  for (iter.t in t.start:time.n) {
    if ((iter.t %% ping) == 1) {
      print(paste("Simulating data. t =", iter.t))
    }
    # Run the natural migration module.
    do.call(nat.mig.module, c(list(curr.dt), nat.mig.params))

    # Run the media modules.
    for (iter.media in 1:length(media.names)) {
      existing.var.names <- data.table::copy(names(curr.dt))
      do.call(media.modules[[iter.media]],
              c(list(curr.dt), media.params[[iter.media]]))
      # Since the module updated the data.table without specifying the media
      # name, variables such as "volume" must be updated to "tv.volume".
      var.names <- setdiff(names(curr.dt), existing.var.names)
      new.var.names <- PasteD(media.names[iter.media], var.names)
      # Delete data from the previous time interval to avoid duplicate columns.
      if (iter.t > 1) {
        curr.dt[, (new.var.names) := NULL]
      }
      setnames(curr.dt, var.names, new.var.names)
    }
    curr.dt[,
            total.spend :=
                EvalText(paste(PasteD(media.names, "spend"),
                               collapse = " + "),
                         curr.dt)]
    # Run the sales module.
    do.call(sales.module, c(list(curr.dt), sales.params))

    # Save progress, and stop if done.
    all.dt[[iter.t]] <- copy(curr.dt)
  }
  return(all.dt)
}

#' Initialize data.
#'
#' Initialize the updating \code{data.table} containing current data on each
#' population segment.
#'
#' @param time.index time index to intialize the data table to
#' @return data.table with one column for each state variable (market,
#'   satiation, activity, brand) and one row per each valid combination of
#'   states for the above state variables. Also initializes the population
#'   count at timepoint 1 as 0.
#' @keywords internal

InitStateData <- function(time.index = 0) {

  # Create a copy of the data.table listing all population segments.
  data.dt <- data.table::copy(kAllStates)

  # Set the time index, geo index, and population size
  data.dt[, time.index := time.index][, pop := 0]
  return(data.dt)
}

#' Surface observable data.
#'
#' Extract observable data from the full dataset.
#'
#' @param full.data \code{data.table} of full data. each row contains
#'   information for a particular timepoint + hidden state
#' @param names.const character names of variables with constant values over
#'   the state, to be aggregated by taking the first entry.
#' @param names.sum character names of variables with numeric values, to be
#'   aggregated by \code{sum()}.
#' @return observable data, aggregated over the hidden states as specified.
#'   Variables not included in names.const or names.sum are assumed to be
#'   hidden variables and not surfaced.
#' @keywords internal

SurfaceData <- function(full.data, names.const, names.sum) {

  # Check the full.data argument.
  full.data <- rbindlist(full.data, use.names = TRUE)
  data.table::setkeyv(full.data,
                      c("time.index", colnames(kAllStates)))
  # The data.table keys should be unique
  assertthat::assert_that(identical(full.data, unique(full.data)))

  # Check the arguments specifying names of variables.
  if (missing(names.const) || is.null(names.const)) {
    names.const <- c(
        grep("price|budget", names(full.data), value = TRUE))
  }
  if (missing(names.sum) || is.null(names.sum)) {
    names.sum <- c(grep(paste0("revenue$|^profit$|\\.sales$|",
                               "\\.volume$|",
                               "\\.spend$|\\.imps$|\\.clicks$"),
                        names(full.data), value = TRUE))
  }
  assertthat::assert_that(
      identical(character(), intersect(names.const, names.sum)),
      msg = "The same variable cannot be aggregated in multiple ways.")
  if (!identical(character(),
                 setdiff(names.const,
                         setdiff(names(full.data), key(full.data))))) {
    warning(paste("Cannot aggregate nonexistent or index columns.",
                  "Removing from names.const."))
    names.const <- intersect(names(full.data), names.const)
  }
  if (!identical(character(),
                 setdiff(names.sum,
                         setdiff(names(full.data), key(full.data))))) {
    warning(paste("Cannot aggregate nonexistent or index columns.",
                  "Removing from names.sum."))
    names.sum <- intersect(names(full.data), names.sum)
  }

  # Perform the data aggregation.
  if (identical(character(), names.sum)) {
    vals.sum <-
        unique(full.data[, "time.index", with = FALSE])
  } else {
    vals.sum <- full.data[, lapply(.SD, sum),
                          by = "time.index",
                          .SDcols = names.sum]
  }
  if (identical(character(), names.const)) {
    vals.const <-
        unique(full.data[, "time.index", with = FALSE])
  } else {
    vals.const <- full.data[, lapply(.SD, function(x) x[1]),
                            by = "time.index",
                            .SDcols = names.const]
  }
  surfaced.data <- vals.sum[vals.const]
  return(surfaced.data)
}
