#' Function cleanup to allow passing character, bare names, and list-formatted functions
#'
#' @param funs a function to evaluate inside something else, could be character, bare name, or list
#'
#' @return
#' @export
#'
#' @examples
functionlister <- function(funs) {
  # the list specification of ~ functions
  if (is.list(funs)) {
    funlist <- funs
    # also catches c(barename, barename2), so deal with that.
    if (is.function(funs[[1]])) {
      funnames <- as.character(substitute(funs))
      # if bare names are c(name, name), the c gets included, so cut it
      if(funnames[1] == "c") {funnames <- funnames[2:length(funnames)]}
      names(funlist) <- funnames
    }

  } else if (is.character(funs)) {
    funlist <- mget(funs, inherits = TRUE)
  } else if (is.function(funs)) {
    funlist <- list(funs)
    names(funlist) <- as.character(substitute(funs))
  } else {
    rlang::abort("funs is of unsupported type (bare names, character, or list")
  }

  return(funlist)
}
