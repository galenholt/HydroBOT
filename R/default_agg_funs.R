# Default aggregation statistics ------------------------------------------
# Mostly, these are standard statistics with na.rm set to TRUE by default. But
# we can add more complex functions if needed.

#' Compensating aggregation
#'
#' Doing well in one time/place can compensate for doing poorly elsewhere. Implemented with max(). For binary, this means passing once means passing the aggregated value.
#'
#' @param x values to aggregate
#' @param na.rm default `na.rm = TRUE` finds the aggregation ignoring NA, opposite usual na.rm default `na.rm = FALSE`, which returns NA if there are any NA.
#'
#' @return summarised data
#' @export
#'
CompensatingFactor <- function(x, na.rm = TRUE) {
  y <- Max(x, na.rm = na.rm)
  return(y)
}

#' Limiting aggregation
#'
#' Doing poorly at one time/place negates benefits elsewhere. Implemented with min(). For binary, this means failing anywhere means failing everywhere.
#'
#' @inheritParams CompensatingFactor
#'
#' @return summarised data
#' @export
#'
LimitingFactor <- function(x, na.rm = TRUE) {
  y <- Min(x, na.rm = na.rm)
  return(y)
}

#' Geometric mean aggregation
#'
#' Finds the geometric mean of the values.
#'
#' @inheritParams CompensatingFactor
#'
#' @return summarised data
#' @export
#'

GeometricMean <- function(x, na.rm = TRUE) {
  y <- exp(mean(log(x), na.rm = na.rm))
  return(y)
}

#' Arithmetic mean aggregation
#'
#' Finds the arithmetic mean of the values. Lightweight wrapper of `mean` with `na.rm = TRUE` to match other functions.
#'
#' @inheritParams CompensatingFactor
#'
#' @return summarised data
#' @export
#'

ArithmeticMean <- function(x, na.rm = TRUE) {
  y <- mean(x, na.rm = na.rm)
  return(y)
}

#' Weighted mean by `area` column
#'
#' Finds the arithmetic mean weighted by the `area` column, which is auto-created in the spatial datasets.
#'
#' @inheritParams CompensatingFactor
#'
#' @return summarised data
#' @export
#'

SpatialWeightedMean <- function(x, na.rm = TRUE) {
  y <- stats::weighted.mean(x = x, w = area, na.rm = na.rm)
}

#' Length aggregation
#'
#' Finds the number of values. Lightweight wrapper of `length` with removal of NA values to match other functions.
#'
#' @inheritParams CompensatingFactor
#'
#' @return summarised data
#' @export
#'

NumberOfValues <- function(x, na.rm = TRUE) {
  if(na.rm == TRUE){
    y <- length(x[!is.na(x)])}
  else if(na.rm == FALSE){
    y <- length(x)}
  return(y)
}

#' Variance  aggregation
#'
#' Finds the variance of the values. Lightweight wrapper of `var` with `na.rm = TRUE` to match other functions.
#'
#' @inheritParams CompensatingFactor
#'
#' @return summarised data
#' @export
#'

Variance <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, var(x, na.rm = TRUE))
}

#' Sum  aggregation
#'
#' Finds the sum of the values. Lightweight wrapper of `sum` with `na.rm = TRUE` to match other functions.
#'
#' @inheritParams CompensatingFactor
#'
#' @return summarised data
#' @export
#'

Sum <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
}

#' Max aggregation with na.rm = TRUE by default *except* that all-NA vectors still return NA, not 0.
#'
#' @inheritParams CompensatingFactor
#'
#' @return numeric
#' @export
#'

Max <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
}

#' Min aggregation with na.rm = TRUE by default *except* that all-NA vectors still return NA, not 0.
#'
#' @inheritParams CompensatingFactor
#'
#' @return numeric
#' @export
#'

Min <- function(x, na.rm = TRUE) {
  ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))
}
