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
#' @return
#' @export
#'
#' @examples
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
#' @return
#' @export
#'
#' @examples
relative <- function(x,y, add_eps = 0) {
  if (add_eps == 'auto') {
    allvals <- c(x,y)
    add_eps <- min(allvals[allvals > 0],
                   na.rm = TRUE)/2
  }
  (x + add_eps)/(y + add_eps)
  }
