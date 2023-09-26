#' Get the outputs from the EWR and clean them up
#'
#' *NOTE* this expects output from the EWRs to be processed through the toolkit
#' functions for cleaning and saving immediately after creation, which may
#' change now that `py-ewr` has changed some things.
#'
#' @param dir path to directory with the EWR output for all gauges and scenarios
#' @param type "annual" or "summary" or "both" do we want the summary results or the annual? If "both", returns a named list of two tibbles instead of a tibble
#' @param gaugefilter character vector of gauge numbers to include. Default `NULL` includes all
#' @param scenariofilter character vector of scenario names to include. Default `NULL` includes all
#'
#' @return a tibble of EWR results if `type == "annual"` or `type == "summary"`. A named list of those tibbles if `type == "both"`
#' @export
#'
#' @examples

get_ewr_output <- function(dir, type,
                           gaugefilter = NULL, scenariofilter = NULL) {

  # assumes files are csvs.
  gaugefiles <- list.files(dir, pattern = '.csv',
                           full.names = TRUE, recursive = TRUE)

  # separate into annual and summary files
  relevantfiles <- gaugefiles[stringr::str_which(gaugefiles, pattern = type)]

  # cut to requested gauges or scenarios
  if (!is.null(gaugefilter)) {
    relevantfiles <- relevantfiles[stringr::str_which(relevantfiles,
                                             pattern = stringr::str_flatten(gaugefilter, collapse = '|'))]
  }

  if (!is.null(scenariofilter)) {
    relevantfiles <- relevantfiles[stringr::str_which(relevantfiles,
                                             pattern = stringr::str_flatten(scenariofilter, collapse = '|'))]
  }

  # read into one df
  ewrdata <- foreach::foreach(i = relevantfiles,
                    .combine = dplyr::bind_rows) %do% {

                      temp <- readr::read_csv(i, col_types = readr::cols())
                    }

  # There are sometimes wholly-blank columns that are read as NA, but should be
  # numeric. We can't pre-set them with readr because they may be in different
  # places for different gauges
  ewrdata <- ewrdata %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.logical), as.numeric))

  # cleanup names and structure
  if (type == 'annual') {
    ewrdata <- suppressWarnings(cleanewrs(ewrdata))
  } else if (type == 'summary') {
    ewrdata <- suppressWarnings(cleanSummary(ewrdata))
  } else if (type == 'both') {
    ewrsum <- suppressWarnings(cleanSummary(ewrdata))
    ewrann <- suppressWarnings(cleanewrs(ewrdata))
    ewrdata <- list(summary = ewrsum, annual = ewrann)
  } else {
    stop('unsupported `type` argument')
  }


  return(ewrdata)

}


# Clean up the EWRs


#' Clean up and standardise names and column types
#'
#' The new format of the EWR output makes this much lighter than previously.
#'
#' @param ewrdf
#'
#' @return a tibble of EWR outputs with cleaned up names, separated ewr_code_timing, and gauges as characters
#' @export
#'
#' @examples
cleanewrs <- function(ewrdf) {
  # Gauges should be characters
  ewrdf$gauge <- as.character(ewrdf$gauge)

  names(ewrdf) <- nameclean(names(ewrdf))

  ewrdf <- ewrdf %>%
    tidyr::separate(ewr_code, into = c("ewr_code", "ewr_code_timing"), sep = "_", remove = FALSE)

  return(ewrdf)
}



#' Clean the incoming summary EWRs
#'
#' There is some data organisation that happens for the toolkit in the scenario
#' controller, but this does a bit more cleaning of data format for further
#' analysis
#'
#' @param summarydf incoming tibble of EWRs after read-in
#'
#' @return tibble reformatted and cleaned for ongoing analysis
#' @export
#'
#' @examples
cleanSummary <- function(summarydf) {

  summarydf <- cleanewrs(summarydf)

  summarydf <- summarydf %>%
    dplyr::mutate(ewr_achieved = target_frequency <= frequency)
}



# Helper to sort the names
nameclean <- function(charvec) {
  cleannames <- charvec %>%
    stringr::str_replace_all('([A-Z])', '_\\1') %>%
    tolower() %>%
    stringr::str_replace_all(pattern = ' ', replacement = '_') %>%
    stringr::str_replace_all(pattern = '-', replacement = '')


}
