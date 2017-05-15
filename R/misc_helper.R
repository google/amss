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

#' Define the Hill transformation function.
#'
#' The Hill function is one option for parameterizing a flexible set of S-shaped
#' curves.
#'
#' @param x original input.
#' @param ec effective concentration parameter.
#' @param slope slope parameter
#' @param beta vertical scale parameter, defaults to 1
#' @return tranformed value = beta / (1 + (x / ec) ^ (-slope))
#' @keywords internal

HillTrans <- function(x, ec, slope, beta = 1) {

  # check input
  assertthat::assert_that(is.numeric(x))

  # If x is vector, do calculation.
  if (is.null(dim(x))) {
    assertthat::assert_that(is.numeric(ec), ec >= 0)
    assertthat::assert_that(is.numeric(slope))
    assertthat::assert_that(is.numeric(beta))
    return(beta / (1 + (x / ec) ^ (-slope)))
  }

  # Else, x has columns. Calculate HillTrans for each column.
  x <- as.matrix(x)
  ec <- CheckLength(ec, ncol(x))
  slope <- CheckLength(slope, ncol(x))
  beta <- CheckLength(beta, ncol(x))
  return.val <- as.matrix(sapply(
      1:ncol(x),
      function(iter) HillTrans(x[, iter], ec[iter], slope[iter], beta[iter])))
  colnames(return.val) <- colnames(x)
  return(return.val)
}

#' Paste with "." separator.
#'
#' Function to simplify pasting with . as separator.
#'
#' @param ... vector of strings to concatenate.
#' @return concatenated string.
#' @keywords internal

PasteD <- function(...) {

  paste(..., sep = '.')
}

#' Parse text string.
#'
#' Shorthand for parse(text = ...), frequently used with data.table This
#' saves us from having to redo row selection in the env argument of
#' EvalText().
#'
#' @param text string to be sent as \code{text} argument to \code{parse}.
#' @return the text as an expression, result of calling
#'   \code{parse(text = text)}.
#' @keywords internal
#'
#' @examples
#' dt <- data.table::as.data.table(mtcars)
#' colname <- "mpg"
#' dt[1:5, eval(amss:::ParseT(colname))]
#' dt[, (amss:::PasteD(colname, "plus1")) := eval(amss:::ParseT(colname)) + 1]
#'
#' @note
#' Including outer \code{eval()} call in the shorthand caused errors,
#' and needs to be done separately.

ParseT <- function(text) {

  return(parse(text = text))
}

#' Parse and evaluate a text string.
#'
#' Shorthand for eval(parse(text = x, env)), frequently used with data.table,
#' with env set to the data.table.
#'
#' @param x string to be sent as \code{text} argument to \code{parse}.
#' @param env environment to use for evaluation. the default parent.frame()
#'   refers to the calling environment. See note for further details.
#' @return the evaluated expression \code{eval(parse(text = x, env))}.
#' @keywords internal
#'
#' @examples
#' # Example 1
#' dt <- data.table::as.data.table(mtcars)
#' colname <- "mpg"
#' dt[, amss:::EvalText(colname, dt)]
#' dt[, (amss:::PasteD(colname, "plus1")) := amss:::EvalText(colname, dt) + 1]
#'
#' # Example 2.
#' x <- 1
#' fn <- function() {
#'   x <- 2
#'   return(c(amss:::EvalText("x"), amss:::EvalText("x", parent.frame())))
#' }
#' fn()
#'
#' @note
#' \code{parent.frame()} is a good default for env, but may lead to unusual
#' behavior if passed to \code{EvalText()} explicitly. Default arguments are
#' evaluated inside the execution environment of the function where they
#' are used. However, explicitly passed arguments are evaluated inside the
#' calling environment instead. Thus, in Example 2, \code{EvalText("x")}
#' evaluates \code{parent.frame()} from within the execution environment of
#' \code{EvalText()}, and finds the execution environment of \code{fn()}, where
#' \code{x = 2}. This is the generally desired behavior. However,
#' \code{EvalText("x", parent.frame())} evaluates \code{parent.frame()} from
#' within the calling environment of \code{EvalText()}, i.e. from within the
#' execution environment of \code{fn()}. This gives the global environment,
#' where \code{x = 1}.

EvalText <- function(x, env = parent.frame()) {

  return(eval(parse(text = x), env))
}

#' Check and adjust object length.
#'
#' Function that checks length of x. It may throw a stop(), warning(), and/or
#' repeat the variable to create the desired length.
#'
#' @param x object to check/adjust length of
#' @param len desired length of object
#' @param hard.stop if TRUE, stop when length(x) is neither 1 nor len. else,
#'   throw a warning and use repeat() to adjust length of x.
#' @param warn.single if TRUE, through a warning when length(x) is 1 and it is
#'   being extended to a new length.
#' @param par.name Character, used in the warning or stop message to indicate
#'   which parameter this warning is meant for.
#' @return x updated to have length len, or throws exception
#' @keywords internal

CheckLength <- function(x, len, hard.stop = TRUE, warn.single = FALSE,
                        par.name = character()) {

  # If x has the correct length, keep it as is.
  if (length(x) == len) {
    return(x)
  }

  # If x has length 1, repeat it to the desired length.
  if (length(x) == 1L) {
    if (isTRUE(warn.single)) {
      warning(paste(c("repeating argument", par.name,
                      "from single value to vector of requested length."),
                    collapse = " "))
    }
    return(rep(x, len))
  }

  # Otherwise, signal a problem with the vector length.
  if (hard.stop) {
    stop(paste(c("Argument", par.name, "of incorrect length"),
               collapse = " "))
  }
  warnings(paste(c("Argument", par.name,
                   "of incorrect length. repeating/truncating."),
                 collapse = " "))
  return(rep(x, length.out = len))
}

#' Calculate the product of population segmentation dimension-specific factors.
#'
#' Multiplies factors corresponding to dimensions of segmentation.
#'
#' @param multiplicand.list list of named numeric vectors. Each vector
#'   corresponds to the named dimension of population segmentation. It
#'   specifies the value assigned to every state possible in that
#'   dimension. Dimensions other than 'activity', 'favorability',
#'   'loyalty', and 'availailability' are ignored. Dimensions missing from
#'   the list do not affect the product.
#' @param starting.multiplicand numeric constant, additional factor to multiply
#'   every product by. Default 1.
#' @return numeric vector. For each population segment, the vector holds the
#'   product of multiplying all factors corresponding to that segment.
#' @keywords internal

MultiplyBySegment <- function(
    multiplicand.list = list(), starting.multiplicand = 1) {

  # Check parameters.
  CheckListNames(multiplicand.list)
  assertthat::assert_that(all(sapply(multiplicand.list, is.numeric)))
  assertthat::assert_that(is.numeric(starting.multiplicand))

  if (length(multiplicand.list) == 0) {
    return(starting.multiplicand)
  }

  # Perform multiplication.
  for (iter.dim in names(multiplicand.list)) {
    multiplicand.list[[iter.dim]] <-
        multiplicand.list[[iter.dim]][kAllStates[[iter.dim]]]
  }
  return(Reduce(`*`, multiplicand.list, starting.multiplicand))
}

#' Read entry from repeated vector.
#'
#' Read an entry of a vector, under the assumption that it repeats to the
#' necessary length.
#'
#' @param v the vector being read
#' @param idx the index to read the value from.
#' @return the \code{idx}-th entry of \code{v}, under the assumption that
#'   \code{v} repeats as necessary to reach length \code{idx}.
#' @keywords internal

ReadRepeatingVector <- function(v, idx) {

  assertthat::assert_that(is.numeric(idx), idx >= 1)
  return(v[(idx - 1) %% length(v) + 1])
}

#' Capitalize the first letter of a string.
#'
#' Capitalize the first letter of a string. This function is meant for single
#' strings only.
#'
#' @param s the string to be capitalized.
#' @return the string \code{s} with its first letter capitalized.
#' @keywords internal

Capitalize <- function(s) {

  assertthat::assert_that(assertthat::is.string(s))
  paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}

#' Check the names of a list.
#'
#' Check that a list has names, and that all these names are valid.
#'
#' @param l the list to be checked.
#' @param valid.names a character vector of valid nmaes.
#' @return This function checks the value of \code{l} and will signal an error
#'   if the check fails. Else, it returns \code{TRUE}.
#' @keywords internal

CheckListNames <- function(l, valid.names = colnames(kAllStates)) {

  assertthat::assert_that(is.list(l))
  assertthat::assert_that(is.character(valid.names))
  if (length(l) > 0) {
    assertthat::assert_that(!is.null(names(l)))
    assertthat::assert_that(all(names(l) %in% valid.names))
  }
  return(TRUE)
}
