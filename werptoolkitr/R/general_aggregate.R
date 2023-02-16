#' Aggregate data along theme dimension
#'
#' This is the core aggregation function for all the aggregation types. it's
#' really a fairly lightweight wrapper over a small dplyr::group_by and
#' summarise to handle passing in grouping columns, aggregation columns, and
#' functions as arguments and handle naming. It assumes no spatial information
#' to deal with. Preparing the data to have the proper grouping columns is the
#' job of the outer calling functions (or the user)
#'
#' @param data a dataframe or tibble with data to aggregate
#' @param groupers an expression for the columns to use as grouping variables
#'   for the aggregation (see `selectcreator` for formats)
#' @param aggCols an expression for the columns to aggregate (the data columns).
#'   See `selectcreator` for formats
#' @param funlist a list of functions and their arguments used to aggregate the
#'   data. See `functionlister` for creation in many cases.
#' @param prefix character prefix for the column name. Default `"agg_"`, but
#'   often better to use the aggregation step. Typically set by particular
#'   calling function to give it the type of aggregation
#' @param failmissing logical, default `TRUE`: fail if the requested grouping or
#'   aggregation columns not exist. If `FALSE`, proceed with those that do exist
#'   and silently drop those that don't. Similar to `tidyselect::all_of()` vs
#'   `tidyselect::any_of()` in `tidyselect`
#' @param ... arguments passed to the aggregation functions. This is *very*
#'   limited, and does not work with data arguments under most conditions.
#'   Almost always better to specify explicitly when building `funlist`, but
#'   works OK with simple functions, e.g. passing `na.rm = TRUE` to mean

#'
#' @return a tibble with columns for the grouping variables and a column of
#'   within-group aggregated values for each `aggCol` and function in `funlist`,
#'   named according to the function applied and original name.
#' @export
#'
#' @examples
#'
general_aggregate <- function(data, groupers,
                              aggCols, funlist,
                              prefix = 'agg_',
                              failmissing = TRUE,
                              ...) {


  # Get the function names differently if bare functions or a list of functions or a character vector
  if (is.list(funlist) | is.character(funlist)) {
    # make funlist a named list whether it comes in that way or as a character vector
    if (is.null(names(funlist))) {
      funnam <- as.character(substitute(funlist))
      if(funnam[1] == "c") {funnam <- funnam[2:length(funnam)]}
    } else {
      funnam <- names(funlist)
    }
    funlist <- functionlister({{funlist}}, forcenames = funnam)

    nameparser = paste0(prefix, '{.fn}_{.col}')
  } else {
    # if funlist is a bare function, leave it alone but get its name
    # https://stackoverflow.com/questions/1567718/getting-a-function-name-as-a-string
    funname <- as.character(substitute(funlist))
    # https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
    nameparser <- paste0(prefix, funname,'_{.col}')
  }

  # Clean up groupers and aggCols from various formats and ensure only present
  # columns are included
  groupers <- selectcreator(rlang::enquo(groupers), data, failmissing)
  aggCols <- selectcreator(rlang::enquo(aggCols), data, failmissing)

  data_agg <- data %>%
    dplyr::group_by(dplyr::across({{groupers}})) %>%
    dplyr::summarise(dplyr::across({{aggCols}}, funlist, ...,
                                   .names = nameparser)) %>%
    dplyr::ungroup()
}

