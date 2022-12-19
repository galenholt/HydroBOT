#' Read in data and aggregate along theme and spatial dimensiont
#'
#' Allows passing only paths to data rather than objects. Wrapper over
#' [prep_ewr_agg()] (though can be made more general once we have other
#' modules), [make_edges()] and [multi_aggregate()]. Particularly useful if we
#' want to pass parameters as strings from a config before anything is read in,
#' and parallelisation.
#' 
#' @inheritParams prep_ewr_agg
#' @inherit multi_aggregate params return
#'
#' @param datpath path to indicator data. Currently needs to be EWR (same as `ewrpath` argument in [prep_ewr_agg()]), but left more general here for future
#' @param causalpath path to the causal relationships .rds file. Should typically be created in `Causal networks`
#' @param ...
#'
#' @export
#'
#' @examples
read_and_agg <- function(datpath, type, 
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
                         ...) {
  
  # The ... pass gauge and scenario filters to `prep_ewr_agg`
  
  data <- prep_ewr_agg(datpath, type = type, geopath = geopath, ...)
  
  # assume theme agg is character, spatial is sf
  themeseq <- aggsequence[purrr::map_lgl(aggsequence, is.character)]
  
  edges <- make_edges(dflist = ewr_causal_path, 
                      fromtos = themeseq)
  
  # be aggressive about parsing tidyselect into characters or expressions get
  # lost in the stack
  groupers <- selectcreator(enquo(groupers), data, failmissing)
  aggCols <- selectcreator(enquo(aggCols), data, failmissing)
  
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
  
  return(aggout)
}
