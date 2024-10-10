# default functions for comparisons. These are mostly just easily-callable
# wrappers for basic arithmetic, though they could be more complex. The main
# thing is that they always have the data argument first as x and the reference
# argument second as y.

#' Difference
#'
#' Easily-callable wrapper for `-`
#'
#' @param x data to compare
#' @param y reference data
#'
#' @return numeric, as `-`
#' @export
#'

difference <- function(x,y) {x-y}

# Relative provides an add_eps argument to prevent divide by zero. need to add
# to both, because otherwise at low flows the others drop below when they
# shouldn't.

#' Division (relative comparison)
#'
#' Easily-callable wrapper for `/`, with ability to avoid divide-by-0
#'
#' @param x data to compare
#' @param y reference data
#' @param add_eps default 0. epsilon to add to prevent divide-by-zero infs. Can be `'auto'` to use half of the minimum nonzero value
#'
#' @return numeric, as `/`
#' @export
#'

relative <- function(x,y, add_eps = 0) {
  if (add_eps == 'auto') {
    allvals <- c(x,y)
    add_eps <- min(allvals[allvals > 0],
                   na.rm = TRUE)/2
  }
  (x + add_eps)/(y + add_eps)
}


#' Odds ratio
#'
#' @param x data to compare
#' @param y reference data
#' @param add_eps default 0. epsilon to add to prevent divide-by-zero infs. Can
#'   be `'auto'` to use half of the minimum nonzero value. Adjusts zeros up and
#'   ones down, but nothing else.
#'
#' @return numeric
#' @export
#'

oddsratio <- function(x,y, add_eps = 0) {
  if (any(x > 1 | x < 0 | y > 1 | y < 0)) {
    rlang::abort("odds ratios need data on 0-1")
  }
  if (add_eps == 'auto') {
    # get the minimum
    allvals <- c(x,y)
    add_eps <- min(allvals[allvals > 0],
                   na.rm = TRUE)/2
  }
  x[x == 0] <- add_eps
  y[y == 0] <- add_eps
  x[x == 1] <- 1-add_eps
  y[y == 1] <- 1-add_eps
  (x / (1-x)) / (y / (1-y))
}
