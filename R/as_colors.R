#' Make a vector of colors have class 'colors'
#'
#' This *must* exist somewhere, because other packages make things with class 'colors', but when I c() to them it breaks.
#'
#' @param colvec character vector of colors
#'
#' @return object of class colors
#'
as_colors <- function(colvec) {
  class(colvec) <- 'colors'
  return(colvec)
}
