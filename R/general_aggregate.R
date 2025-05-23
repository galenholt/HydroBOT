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
#'   data. See `functionlister` for creation in many cases. The situation with a
#'   bare anonymous function, e.g. `~mean(., na.rm = T)` is not supported
#'   because we need a name. Use a named list if using anonymous functions, e.g.
#'   `list(mean = ~mean(., na.rm = T))`. *If using functions with a
#'   data-variable argument*, e.g. weighted.mean with a column of weights, we
#'   now (as of `dplyr` 1.1) have some workarounds. One option is if specified
#'   *as a function argument*, the function can just go in as a bare name or
#'   anonymous. If specified elsewhere, it can be wrapped in [rlang::quo()],
#'   e.g. `agglist <- rlang::quo(list(mean = mean, wm = ~weighted.mean(.,
#'   weight_column_name, na.rm = T)))`. If it isn't, there is now an internal
#'   workaround to add that on that seems to be stable but may cause unforeseen
#'   issues. This workaround also allows building custom aggregation functions
#'   (not-anonymous) with the data-variable argument either exposed or hardcoded
#'   (see [SpatialWeightedMean()]). The error checks for names do not work for
#'   quosures, so make sure you name the list if using [rlang::quo()].
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
general_aggregate <- function(data, groupers,
                              aggCols, funlist,
                              prefix = 'agg_',
                              failmissing = TRUE,
                              ...) {


  # Clean up groupers and aggCols from various formats and ensure only present
  # columns are included as character vectors. We're in this mess because some
  # of the rlang breaks with depth
  groupers <- selectcreator(rlang::enquo(groupers), data, failmissing)
  aggCols <- selectcreator(rlang::enquo(aggCols), data, failmissing)

  if (!is.character(groupers) || !is.character(aggCols)) {
    rlang::abort('the new way of enforcing characters is not working, we have tidyselect still. back to `{{}}` in the `across`')
  }

  # typical name parsing
  nameparser = paste0(prefix, '{.fn}_{.col}')

  # if a quosure, just do the processing and return
  if (rlang::is_quosure(funlist)) {
    data_agg <- data |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(groupers))) |>
      dplyr::summarise(dplyr::across(tidyselect::all_of(aggCols), !!funlist, ...,
                                     .names = nameparser)) |>
      dplyr::ungroup()

    return(data_agg)
  }

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

  } else {
    # if funlist is a bare function, leave it alone but get its name
    # https://stackoverflow.com/questions/1567718/getting-a-function-name-as-a-string
    funname <- as.character(substitute(funlist))
    # https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
    nameparser <- paste0(prefix, funname,'_{.col}')
  }


  # Try, and if fail, force with characters. Should I just go straight for characters? It seems so clunky I'd rather not.
  data_agg <- try(data |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(groupers))) |>
    dplyr::summarise(dplyr::across(tidyselect::all_of(aggCols), {{funlist}}, ...,
                                   .names = nameparser)) |>
    dplyr::ungroup(),
    silent = TRUE)

  if (inherits(data_agg, 'try-error')) {
    # turn the list into characters. Eval can't handle `return`, so make an
    # attempt to get rid of it, though this is likely fragile.
    if (is.character(funlist)) {
      charfun <- funlist
    } else {
      charfun <- paste0(deparse(funlist), collapse = '')
    }

    # add rlang::quo unless it's already there.
    if (!grepl("quo", charfun)) {
      charfun <- paste0(c("rlang::quo(", charfun, ")"), collapse = '')
    }
    charfun <- stringr::str_remove_all(charfun, 'return\\([A-z]\\)')
    # FUNS2 <- eval(parse(text = charfun)) # base R
    # rlang claims to be faster?
    FUNS_quo <- rlang::eval_tidy(rlang::parse_expr(charfun)) # nocov

    # go again
    data_agg <- data |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(groupers))) |>
      dplyr::summarise(dplyr::across(tidyselect::all_of(aggCols), {{FUNS_quo}}, ...,
                                     .names = nameparser)) |>
      dplyr::ungroup()
  }


  return(data_agg)
}

