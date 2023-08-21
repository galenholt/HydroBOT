#' Create edges dataframe for causal network or theme aggregation
#'
#' Takes a list of causal relationships tables and creates all pairwise
#' relationships between each of the columns specified in the from-to pairs. Can
#' be geographically filtered by gauge or planning unit
#'
#' @param dflist list of tibbles or dataframes containing the matches between
#'   nodes. Single dataframes may hold multiple relationships. List of
#'   dataframes because some relationships are at different scales
#' @param fromtos list of length-2 character vectors `c('from', 'to')`, giving
#'   the column names of the node categories to connect (and implies
#'   directionality).
#' @param fromfilter character vector of nodes to include (a subset of the nodes
#'   in the `from` column. Default `NULL` includes all nodes within the column.
#' @param tofilter  character vector of nodes to include (a subset of the nodes
#'   in the `to` column. Default `NULL` includes all nodes within the column
#' @param gaugefilter character vector of gauge numbers to include. Default
#'   `NULL` includes all gauges
#' @param pufilter character vector of planning units to include. Default `NULL`
#'   includes all planning units
#' @param gaugeplanmatch dataframe of matches between gauges and planning units.
#'   If such a dataframe is not passed (the default, `gaugeplanmatch = NULL`),
#'   the function attempts to find a dataframe in `dflist` that has both and
#'   create the matching internally. `gaugeplanmatch` is used to check
#'   consistency if both `gaugefilter` and `pufilter` are passed, and to get the
#'   other when only one is passed
#' @param extrasave Any other columns to retain from the original datasets that
#'   might be desired attributes later. Default `NULL` retains none.
#'
#' @return a tibble with columns for `gauge` and `PlanningUnitID` (where
#'   included), `from` and `to` columns indicating the directionality of
#'   pairings, `fromtype` and `totype` for the type of node being connected, and
#'   an `edgeorder` column for the order in which the edges were passed
#'   (previously used for plotting, now deprecated).
#' @export
#'
#' @examples
#'
make_edges <- function(dflist,
                fromtos,
                fromfilter = NULL,
                tofilter = NULL,
                gaugefilter = NULL,
                pufilter = NULL,
                gaugeplanmatch = NULL,
                extrasave = NULL) {

  # Ideally gaugeplanmatch will be passed a canonical list. Otherwise this tries
  # to find one

  # Read in a list from a file if just have a path
  if (is.character(dflist)) {
    dflist <- readRDS(dflist)
  }

  # Get names
  dfnames <- purrr::map(dflist, names)
  # Get whether each df has a gauge or planning unit column
  dfgauge <- unlist(purrr::map(dfnames, ~'gauge' %in% .))
  dfpu <- unlist(purrr::map(dfnames, ~'PlanningUnitID' %in% .))

  # We can use that to try to generate a matching gauge-pu if there wasn't one passed
  if (is.null(gaugeplanmatch) & any(dfgauge & dfpu)) {
    gaugeplanmatch <- dflist[[which(dfgauge & dfpu)[1]]] %>%
      dplyr::select(gauge, PlanningUnitID) %>%
      dplyr::distinct()
  }



  # Go over each fromto pair, find their df and filter and bind
  # them all together


  # I think iter() would let us send only the matching df (i.e.
  # do the thisdf bit right in the call). Deal with that later if it's an issue.
  alledges <- foreach::foreach (p = fromtos, i = iterators::icount(), .combine = dplyr::bind_rows) %do% {

    # Find the right causal sheet
    # If there are multiple sheets, use the first one (No obvious heuristic here, and the index is simpler than anything more complex.)
    dfindex <- which(unlist(purrr::map(dfnames, ~all(p %in% .))))[1]

    if(is.na(dfindex)) {
      rlang::abort(glue::glue("Cannot find causal relationship between {p[1]} and {p[2]}. This tends to be misspellings, but could also be mis-specification of the causal relationships"))
    }

    thisdf <- dflist[[dfindex]]

    # Get the filtering values- this is its own function because it has
    # error-catching and cross-checks
    filterlist <- filtergroups(thisdf,
                               fromcol = p[1], tocol = p[2],
                               fromfilter = fromfilter, tofilter = tofilter,
                               gaugefilter = gaugefilter, pufilter = pufilter,
                               gaugeplanmatch = gaugeplanmatch)



    # these ifs are annoying, needed to filter to the gauge and planning unit only if
    # the columns exist
    if (dfgauge[dfindex]) {
      thisdf <- thisdf %>%
        dplyr::filter(gauge %in% filterlist$gaugefilter)
    }

    if (dfpu[dfindex]) {
      thisdf <- thisdf %>%
        dplyr::filter(PlanningUnitID %in% filterlist$pufilter)
    }

    # we can assume the fromto exist
    thisdf <- thisdf %>%
      dplyr::filter(.data[[p[1]]] %in% filterlist$fromfilter) %>%
      dplyr::filter(.data[[p[2]]] %in% filterlist$tofilter) %>%
      dplyr::rename(from = p[1], to = p[2]) %>%
      dplyr::select(tidyselect::any_of(c('gauge', 'PlanningUnitID', extrasave, 'from', 'to', 'color'))) %>%
      dplyr::mutate(fromtype = p[1],
             totype = p[2],
             edgeorder = i) %>% # I was using this to set the nodeorder, but dropping that.
      dplyr::distinct() # kill duplicates


  }

  return(alledges)

}


# helper function to filter edge dfs. Primarily deals with nulls and cross-checking and error-catching in the from-tos

filtergroups <- function(edgedf,
                         fromcol, tocol,
                         fromfilter = NULL,
                         tofilter = NULL,
                         gaugefilter = NULL,
                         pufilter = NULL,
                         gaugeplanmatch = NULL) {

  # These don't enforce column names, so use select to allow accepting a character
  if (is.null(fromfilter)) {
    fromfilter <- edgedf %>% dplyr::select(tidyselect::all_of(fromcol)) %>% dplyr::distinct() %>% dplyr::pull()
  }

  if (is.null(tofilter)) {
    tofilter <- edgedf %>% dplyr::select(tidyselect::all_of(tocol)) %>% dplyr::distinct() %>% dplyr::pull()
  }

  # Gauges and planning units
  # Get matching values if a matching df is passed in as an argument
  if (!is.null(gaugefilter) & !is.null(gaugeplanmatch)) {
    pfromg <- gaugeplanmatch %>%
      dplyr::filter(gauge %in% gaugefilter) %>%
      dplyr::select(PlanningUnitID) %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }

  if (!is.null(pufilter) & !is.null(gaugeplanmatch)) {
    gfromp <- gaugeplanmatch %>%
      dplyr::filter(PlanningUnitID %in% pufilter) %>%
      dplyr::select(gauge) %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }

  # doesn't need to be nested
  if (is.null(gaugefilter) | is.null(pufilter)) {
    if (is.null(gaugeplanmatch)) {
      warning("Unable to cross-check gauges and planning units, trusting the user they work together")
    }
  }

  # Now build and check

  # If planning unit passed, but not gauge, use the gauges from the planning unit
  if (is.null(gaugefilter) & !is.null(pufilter)) {
    gaugefilter <- gfromp
  } else if (!is.null(gaugefilter) & is.null(pufilter)) {
    # If gauge passed, but not pu, use the pu from the gauges
    pufilter <- pfromg
  } else if (!is.null(gaugefilter) & !is.null(pufilter)) {
    # check compliance, but let run through with a warning
    gtop <- all(gaugefilter %in% gfromp)
    ptog <- all(pufilter %in% pfromg)

    # Could get fancier here and let us know which way things are failing- e.g.
    # "you have chosen x out of y gauges within planning unit a, and gauge z
    # falls outside the selected planning units
    if (!(gtop & ptog)) {
      warning("Filtering to requested gauges and planning units, but they do not fully match")
    }
  } else if (is.null(gaugefilter) & is.null(pufilter)) {
    # if neither filter given, use all of both, if the columns exist
    if ('gauge' %in% names(edgedf)) {
      gaugefilter <- unique(edgedf$gauge)
    }
    if ('PlanningUnitID' %in% names(edgedf)) {
      pufilter <- unique(edgedf$PlanningUnitID)
    }


  }

  return(tibble::lst(fromfilter = fromfilter,
                     tofilter = tofilter,
                     gaugefilter = gaugefilter,
                     pufilter = pufilter))
}


