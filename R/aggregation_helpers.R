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

    # we actually want the names of the functions. That's easy if they come in as characters, but not if they come in bare or a lambda function or ...

    fs <- purrr::map_lgl(funsequence, is.character)
    if (any(!fs)) {
      rlang::inform(c("Attempting to deparse lambda function in funsequence. ",
      "i" = "It is better to use named functions than lambdas in the funsequence, because they are more clearly defined and can be more easily known later."))
      funsequence[!fs] <- names(unlist(funsequence[!fs]))
    }

    # suppressWarnings probably a bad idea, but there's a lot of really
    # pointless warnings that get thrown
    suppressWarnings(
      aggincols <- aggdf |>
        tidyr::pivot_longer(tidyselect::ends_with(aggCols)) |>
        # This removes the aggregation level name ('ewr_code', 'catchment', etc)
        dplyr::mutate(
          allfuns = stringr::str_remove_all(
            name,
            stringr::str_flatten(
              stringr::str_c("(", unlist(aggsequence), ")"),
              collapse = "|"
            )
          ),
          allfuns = stringr::str_remove(allfuns, "^_")
        ) |>
        tidyr::separate(
          allfuns,
          into = paste0("aggfun_", length(funsequence):1),
          sep = "__"
        ) |>
        tidyr::separate(
          aggfun_1,
          into = c("aggfun_1", "original"),
          sep = "_",
          extra = "merge"
        ) |>
        dplyr::mutate(alllevs = stringr::str_remove_all(
          name,
          stringr::str_flatten(stringr::str_c("(", unlist(funsequence), ")"),
            collapse = "|"
          )
        )) |>
        tidyr::separate(
          alllevs,
          into = paste0("aggLevel_", length(funsequence):1),
          sep = "__"
        ) |>
        dplyr::select(-name) |>
        # sf seems to be behind tidyverse, and needs characters here instead of bare
        # names. Both seem to work for normal df/tibbles
        tidyr::pivot_wider(names_from = "original", values_from = "value") |>
        dplyr::select(
          !tidyselect::ends_with(stringr::str_c("_", as.character(
            1:length(funsequence)
          ))),
          tidyselect::ends_with(stringr::str_c("_", as.character(
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
    xg <- mget(x, inherits = TRUE, ifnotfound = x)
    xg <- xg[[1]]
    # only actually return the result of the get if it's an sf
    if (inherits(xg, 'sf')) {
      return(xg)
    } else {
      return(x)
    }
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
  if (inherits(x, "data.frame")) {
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
    charfun <- paste0(c("rlang::quo(", charfun, ")"), collapse = "")
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


# Parsing group_until -----------------------------------------------------
#' Parses group_until into standard form
#'
#' @inheritParams multi_aggregate
#'
#' @return a named list of the group_until groupers with numeric index.

parse_group_until <- function(group_until, groupers, aggsequence) {
  # get indexing for group_until drops
  if (!is.list(group_until)) {
    # Try to fix, but not too hard
    if (is.character(groupers) & length(group_until) == length(groupers)) {
      group_until <- as.list(group_until) |> setNames(groupers)
    } else {
      rlang::abort("group_until should always be a named list.
      If used with types other than character or numeric, it *must* be.")
    }
  }

  # Try a bit harder with the names; they fall off for unnamed vectors with functions
  if (is.null(names(group_until)) & length(group_until) == length(groupers) & is.character(groupers)) {
    names(group_until) <- groupers
  }

  group_indices <- purrr::map(group_until, \(x) parse_aggnum(x, aggsequence)) |>
    purrr::discard(is.null)

  return(group_indices)
}

#' Main function of parse_group_until, finds the index for different ways of specifying group_until
#'
#' @param x one of the group_until items
#' @param aggsequence as in [multi_aggregate()]
#'
#' @return numeric index for each group_until
#'
parse_aggnum <- function(x, aggsequence) {
  # if na, make grouper persist (if we test a closure it `warn`s)
  if (!rlang::is_function(x) && is.na(x)) {
    # gind <- length(aggsequence) + 1
    gind <- NULL
  } else {
    # the is.na in an outer level, because we can have NA characters and numeric and etc
    # if character, find the aggsequence name
    if (is.character(x)) {
      if (!x %in% names(aggsequence)) {
        rlang::warn("not able to infer `group_until`, aggsequence names do not match.
                    Retaining grouping until the end")
        gind <- length(aggsequence) + 1
      } else {
        gind <- which(names(aggsequence) == x)
      }
    }
    if (rlang::is_function(x)) {
      # assumes to evaluate to logical
      gind <- min(which(aggsequence |> purrr::map_lgl(x)))
    }

    if (is.numeric(x)) {
      gind <- x
    }
  }

  return(gind)
}


#' Helper to identify the dimension of each step in aggsequence
#'
#' @inheritParams multi_aggregate
#'
#' @return character vector with 'spatial', 'theme' or 'temporal' for each step in aggsequence
#' @export
#'
identify_dimension <- function(aggsequence, causal_edges) {

  if (inherits(causal_edges, 'data.frame')) {
    causalnames <- names(causal_edges)

    # If they're edges, they already have been lengthened
    if ('fromtype' %in% causalnames) {
      causalnames <- c(unique(causal_edges$fromtype), unique(causal_edges$totype))
    }

  } else {
    causalnames <- purrr::map(causal_edges, names) |> unlist()
  }

  spatialsteps <- purrr::map_lgl(aggsequence, is_sf)
  themesteps <- purrr::map_lgl(aggsequence, \(x) is_theme(x, causalnames))
  timesteps <- purrr::map_lgl(aggsequence, is_time)

  checkassign <- purrr::pmap_int(list(spatialsteps, themesteps, timesteps), sum)

  # Check we haven't duplicated or missed anything
  if (any(checkassign != 1)) {
    rlang::abort(c("Cannot infer dimension of aggsequence.",
                   glue::glue("step(s) {names(checkassign)[checkassign > 1]} are assigned to multiple dimensions"),
                   glue::glue("step(s) {names(checkassign)[checkassign < 1]} are not assigned to any dimension")))
  }

  # get a single vector of the dims
  steptype <- vector(mode = 'character', length = length(aggsequence))
  steptype[spatialsteps] <- 'spatial'
  steptype[themesteps] <- 'theme'
  steptype[timesteps] <- 'temporal'

  return(steptype)

}

#' Test whether a list-item specifies a theme dimension
#'
#' Intended to be purrr-ed over from aggsequence
#'
#' @param x list-item
#' @param causalnames names of the columns in a causal network
#'
#' @return logical
is_theme <- function(x, causalnames) {
  all(inherits(x, 'character') &
    length(x) == 2 &
    x %in% causalnames)
}

#' Test whether a list-item is time, including using [base::cut.Date()] specifications
#'
#' Intended to be purrr-ed over from aggsequence
#'
#' @param x a list-item
#'
#' @return Logical
is_time <- function(x) {
  if (all(inherits(x, 'POSIXt') | inherits(x, 'Date'))) {
    return(TRUE)
  } else if (inherits(x, 'character') &&
             length(x) == 1 &&
             grepl("sec|min|hour|day|DSTday|day|week|month|quarter|year|all_time", x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Test whether an object is an sf
#'
#' @param x anything, but typically a dataframe
#'
#' @return Logical
#' @export
#'
#' @examples
#' is_sf(basin)
#' is_sf(iris)
is_sf <- function(x) {
  inherits(x, "sf")
}

#' Test whether an object is a spatial point sf
#'
#' The data has to be both an sf and then only contain points
#'
#' @param x dataframe or other object
#'
#' @return logical, TRUE if is sf and all data are POINTs
#' @export
#'
is_point <- function(x) {
  if (is_sf(x)) {
    if (all(sf::st_is(x, "POINT"))) {
      return(TRUE)
    }
  }
  return(FALSE)
}


#' Test whether an object is an sf other than a point
#'
#' This is not just !is_point(), because it still needs to be an sf.
#'
#' @param x dataframe or other object
#'
#' @return logical, TRUE if sf and not all data are POINTs
#' @export
#'
is_notpoint <- function(x) {
  if (is_sf(x)) {
    if (!all(sf::st_is(x, "POINT"))) {
      return(TRUE)
    }
  }
  return(FALSE)
}
