# Default aggregation statistics ------------------------------------------
# Mostly, these are standard statistics with na.rm set to TRUE by default. But
# we can add more complex functions if needed.

CompensatingFactor <- function(x, na.rm = TRUE) {
  y <- max(x, na.rm = na.rm)
  return(y)
}

LimitingFactor <- function(x, na.rm = TRUE) {
  y <- min(x, na.rm = na.rm)
  return(y)
}

GeometricMean <- function(x, na.rm = TRUE) {
  y <- exp(mean(log(x), na.rm = na.rm))
  return(y)
}

ArithmeticMean <- function(x, na.rm = TRUE) { 
  y <- mean(x, na.rm = na.rm)
  return(y)
}

