#' Helper to handle different ways we might choose columns
#'
#' Tidyselect has a robust way of selecting columns, but we often need to do
#' that selection indirectly- ie pass the selecting in as an argument to an
#' outer function. There are a number of ways to do this, but this is reasonably
#' robust and general, relying on using `tidyselect` itself. Allows passing
#' character vectors, bare names, or tidyselect syntax, provided this function
#' is called at the right place in the call stack
#'
#' @param selectvals the selection of columns. Can be character, bare names, or
#'   `tidyselect` syntax, e.g. `tidyselect::starts_with()`. If `tidyselect`, it can include
#'   objects, but they need to be `!!`. For example, if `prefix = 'ABC'`, to
#'   select columns starting with that prefix, should call `selectvals =
#'   tidyselect::starts_with(!!prefix)`
#' @param data a tibble or dataframe to find the columns
#' @param failmissing logical, default `TRUE`: fail if the requested grouping or
#'   aggregation columns not exist. If `FALSE`, proceed with those that do exist
#'   and silently drop those that don't
#' @return a character vector of column names. The intention is to use them in
#'   `dplyr::group_by` or `select` with `dplyr::select(dplyr::across({{output}}))`. The underlying
#'   `eval_select` returns a named integer vector giving column indices, but we
#'   return only the names because in use the indices may not be stable
#'   throughout the calling function(s)
#' @export
#'
#' @examples
selectcreator <- function(selectvals, data, failmissing = TRUE) {
  if (is.character(selectvals)) {
    if (failmissing) {
      s1g <- rlang::expr(tidyselect::all_of(selectvals))
    } else {
      s1g <- rlang::expr(tidyselect::any_of(selectvals))
    }
  } else if (is.language(selectvals)) {
    s1g <- selectvals
  } else {
    stop('selectvals should be characters or defused tidyselect- either `rlang::expr(tidyselect)` in call to function or `rlang::enquo(tidyselect)` internally')
  }

  # a secondary check in case s1g evals to just a character vector. This
  # typically only happens if a character vector is passed in but wrapped in
  # rlang::enquo, since the first conditional wraps incoming bare character
  # vectors in tidyselect already
  if (is.character(rlang::get_expr(s1g))) {
    charvec <- rlang::get_expr(s1g)
    if (failmissing) {
      s1g <- rlang::expr(tidyselect::all_of(charvec))
    } else {
      s1g <- rlang::expr(tidyselect::any_of(charvec))
    }
  }

  # Returning names instead of the indices is safer in case cols get reshuffled
  # at some point as of R 4.2, this throws a warning when s1g evals to a
  # character vector, and needs tidyselect::all_of( or tidyselect::any_of(
  # wrapping- see secondary conditional above
  s1g <- s1g %>%
    tidyselect::eval_select(data, strict = failmissing) %>%
    names()

  return(s1g)
}
