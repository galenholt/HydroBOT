time_aggregate <- function(dat, timebreaks,
                           groupers,
                           aggCols,
                           funlist,
                           geonames, ...) {
  # Bare names get lost as we go down into further functions, so use characters
  # and throw an ugly conditional on to do that. It's extra ugly with multiple bare names.
  # Have to specifically exclude quosures to avoid rlang warning, but this conditional is a mess.
  if (!rlang::is_quosure(funlist) &&
    (is.function(funlist) ||
      (is.list(funlist) &
        is.function(funlist[[1]])))) {
    funlist <- as.character(substitute(funlist))
    if (funlist[1] == "c") {
      funlist <- funlist[2:length(funlist)]
    }
  }
}
