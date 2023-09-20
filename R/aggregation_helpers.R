#' Change aggregation history in col names to stepwise columns
#'
#' By default, aggregation history (function and level) is tracked in the column
#' names for memory purposes and to keep units clear. But it's usually more
#' useful to be able to access that information, so this parses the names of the
#' aggregated columns into columns for the aggregation function at each
#' aggregation level.
#'
#' @param aggdf aggregated dataframe or tibble
#' @param aggsequence a list of the names of the aggregation. works best if the
#'   list passed to the calling function is named, and those names are passed in
#'   here.Typically it will be the second value in each theme aggregation step
#'   (the to-theme), or the name of the spatial polygons. But extracting those
#'   when unnamed isn't well tested.
#' @param funsequence a list of functions (as characters or lists of names and
#'   arguments) to apply to do the aggregation at each step defined in
#'   `aggsequence`. Can be multiple funs per aggregation step.
#' @param aggCols the name(s) of the original columns that were aggregated.
#'
#' @return a tibble with a column named as the original input data at the start
#'   of aggregation, and columns for every aggregation step giving the level
#'   aggregated into and the function used for the aggregation. if multiple
#'   functions are used, the rows are stacked (long format).
#' @export
#'
agg_names_to_cols <-
  function(aggdf, aggsequence, funsequence, aggCols) {
    # suppressWarnings probably a bad idea, but there's a lot of really
    # pointless warnings that get thrown
    suppressWarnings(
      aggincols <- aggdf %>%
        tidyr::pivot_longer(tidyselect::ends_with(aggCols)) %>%
        # This removes the aggregation level name ('ewr_code', 'catchment', etc)
        dplyr::mutate(
          allfuns = stringr::str_remove_all(name,
                                            stringr::str_flatten(
                                              stringr::str_c('(', unlist(aggsequence), ')'),
                                              collapse = '|'
                                            )),
          allfuns = stringr::str_remove(allfuns, '^_')
        ) %>%
        tidyr::separate(
          allfuns,
          into = paste0('aggfun_', length(funsequence):1),
          sep = '__'
        ) %>%
        tidyr::separate(
          aggfun_1,
          into = c('aggfun_1', 'original'),
          sep = '_',
          extra = 'merge'
        ) %>%
        dplyr::mutate(alllevs = stringr::str_remove_all(
          name,
          stringr::str_flatten(stringr::str_c('(', unlist(funsequence), ')'),
                               collapse = '|')
        )) %>%
        tidyr::separate(
          alllevs,
          into = paste0('aggLevel_', length(funsequence):1),
          sep = '__'
        ) %>%
        dplyr::select(-name) %>%
        # sf seems to be behind tidyverse, and needs characters here instead of bare
        # names. Both seem to work for normal df/tibbles
        tidyr::pivot_wider(names_from = 'original', values_from = 'value') %>%
        dplyr::select(
          !tidyselect::ends_with(stringr::str_c('_', as.character(
            1:length(funsequence)
          ))),
          tidyselect::ends_with(stringr::str_c('_', as.character(
            1:length(funsequence)
          )))
        )
    )

    return(aggincols)
  }


# Parsing the aggsequence -------------------------------------------------

#' `get` character names for geographic data, allowing aggsequences that are
#' only characters.
#'
#' Only length-1 entries in aggregation sequence are turned into dataframes.
#' See [multi_aggregate()] for use. Intended to be used with [purrr::map()] or similar.
#'
#' @param x character vector or dataframe, typically one entry in the aggregation list
#'
#' @return aggregation sequence, with length-1 character vectors attempted to `get`
#'
#' No `examples` tag here because we don't export this
#' purrr::map(list(ewr_code = c('ewr_code_timing', 'ewr_code'), sdl_units = "sdl_units"), parse_geo)
parse_geo <- function(x) {
  if (length(x) == 1 & is.character(x)) {
    return(get(x))
  } else {
    return(x)
  }
}

#' Get an all-character version of the aggsequence by parsing entries that are
#' dataframes back to their names
#'
#'  Intended to be used with [purrr::imap()].
#'
#' @param x character vector or dataframe, typically one entry in the aggregation list
#' @param idx name of the list item
#'
#' @return character vector
#'
#' No `examples` tag because we don't export this
#' purrr::imap(list(ewr_code = c('ewr_code_timing', 'ewr_code'),
#' sdl_units = sdl_units),
#' parse_char)
#'
parse_char <- function(x, idx) {
  if (inherits(x, 'data.frame')) {
    return(idx)
  } else {
    return(x)
  }
}

#' Get an all-character version of the funsequence by parsing entries that are
#' lists or quosures to characters
#'
#'  Intended to be used with [purrr::map()].
#'
#' @param x entry in funsequence, typically character, list, or quosure
#'
#' @return character vector
#'
parse_char_funs <- function(x) {
  if (rlang::is_quosure(x)) {
    # deparsing it removes the quo, so add it back
    charfun <- deparse(rlang::quo_get_expr(x))
    charfun <- paste0(c("rlang::quo(", charfun, ")"), collapse = '')
    return(charfun)
  }
  if (is.character(x)) {
    return(x)
  }
  if (is.list(x)) {
    charfun <- deparse(x)
    return(charfun)
  }

}
