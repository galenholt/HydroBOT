#' Read in data and aggregate along theme and spatial dimensiont
#'
#' Allows passing only paths to data rather than objects. Wrapper over
#' [prep_ewr_agg()] (though can be made more general once we have other
#' modules), [make_edges()] and [multi_aggregate()]. Particularly useful if we
#' want to pass parameters as strings from a config before anything is read in,
#' and parallelisation.
#'
#' @inheritParams multi_aggregate
#' @inherit multi_aggregate params return
#'
#' @param datpath path to indicator data. Currently needs to be EWR (same as
#'   `ewrpath` argument in [prep_ewr_agg()]), but left more general here for
#'   future
#' @param type character of which type of EWR output (currently `'summary'`,
#'   `'annual'`, or `'both'`). New values, e.g. `'all`' probably work but
#'   untested.
#' @param geopath path to the file with gauge locations in lat/long (assumes BOM
#'   currently), or an `sf` with gauge locations
#' @param causalpath path to the causal relationships .rds file. Should
#'   typically be created in `Causal networks`
#' @param returnList default `TRUE`, whether to return the output to the current
#'   session
#' @param savepath default `NULL`, a path to save the output to. Note that this
#'   names the output rds file directly '`type`_aggregated.rds', so the path
#'   should include only the directory structure.  If `NULL`, does not save. If
#'   `savepath = NULL` and `returnList = FALSE`, the function errors to avoid
#'   wasting resources.
#' @param ...
#'
#' @export
#'
#' @examples
read_and_agg <- function(datpath,
                         type,
                         geopath,
                         causalpath,
                         groupers = 'scenario',
                         aggCols,
                         aggsequence,
                         funsequence,
                         saveintermediate = FALSE,
                         namehistory = TRUE,
                         keepAllPolys = FALSE,
                         failmissing = TRUE,
                         returnList = TRUE,
                         savepath = NULL,
                         ...) {

  # The ... pass gauge and scenario filters to `prep_ewr_agg`

  if (!returnList & is.null(savepath)) {
    rlang::abort(message = "not returning output to disk or session. aborting to not use the resources.")
  }

  data <- prep_ewr_agg(datpath, type = type, geopath = geopath, ...)

  # parse any character names for the spatial data, then character will be the themes
  aggsequence <- purrr::map(aggsequence, parse_geo)
  themeseq <- aggsequence[purrr::map_lgl(aggsequence, is.character)]

  edges <- make_edges(dflist = causalpath,
                      fromtos = themeseq)

  # be aggressive about parsing tidyselect into characters or expressions get
  # lost in the stack
  groupers <- selectcreator(rlang::enquo(groupers), data, failmissing)
  aggCols <- selectcreator(rlang::enquo(aggCols), data, failmissing)

  # Annoying how much of this is just pass-through arguments. Could use dots,
  # but then would need to specify the prep_ewr_agg dots.
  aggout <- multi_aggregate(data,
                            edges,
                            groupers = groupers,
                            aggCols = aggCols,
                            aggsequence = aggsequence,
                            funsequence = funsequence,
                            saveintermediate = saveintermediate,
                            namehistory = namehistory,
                            keepAllPolys = keepAllPolys,
                            failmissing = failmissing)

  # use RDS so can read it in to a different name.
  if (!is.null(savepath)) {
    if (!dir.exists(savepath)) {dir.create(savepath, recursive = TRUE)}
    saveRDS(aggout, file.path(savepath, paste0(type, '_aggregated.rds')))
  }

  if (returnList) {return(aggout)} else {return(NULL)}

}
