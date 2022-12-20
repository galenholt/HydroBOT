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
  ewrdata <- foreach(i = relevantfiles,
                    .combine = bind_rows) %do% {

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


#' Clean the OLD STYLE incoming annual EWR results
#'
#' *This function is deprecated- the new annual EWR outputs are much cleaner on
#' read-in. This still exists to allow use of historic versions.* There is some
#' data organisation that happens for the toolkit in the scenario controller,
#' but this does a bit more cleaning of data format for further analysis
#'
#' @param ewrdf incoming tibble of EWRs after read-in
#'
#' @return tibble reformatted and cleaned for ongoing analysis
#' @export
#'
#' @examples
cleanYrEWRs <- function(ewrdf) {

  # Brackets are in some cols (daysBetweenEvents) from Pandas. This is because
  # they are lists of length n (often 0). That's not ideal, but it's unclear
  # what the best way to proceed is when there may be more than one value. For
  # the moment, I'll leave them as characters, which requires doing the pivoting
  # separately and re-joining. List-cols would work here as well, but not sure we need it yet,
  # and we'd have to make everything a list-col if we wanted to avoid the
  # separate pivots.

  # Gauges should be characters
  ewrdf$gauge <- as.character(ewrdf$gauge)

  # The way to do this really is to have character vs numeric cols. But the
  # list-cols are screwing that up. It's not entirely clear how they should be dealt with.
  # we do end up splitting the stats out. So, I can do it two ways, it's just annoying and repetitive
  # wrapping these in names(ewrdf) gives the names, rather than indices.
  codecols <- stringr::str_which(names(ewrdf), '([0-9])|([A-Z])')
  daysbetweencols <- stringr::str_which(names(ewrdf), 'daysBetweenEvents')
  refcols <- which(!(names(ewrdf) %in% names(ewrdf)[codecols]))

  # Split and stack the columns
  gaugelong <- ewrdf %>%
    dplyr::select(-tidyselect::all_of(daysbetweencols)) %>%
    # The cols would be nice to be a more general spec.
    tidyr::pivot_longer(cols = matches('([0-9])|([A-Z])', ignore.case = FALSE), # matches('([0-9])|([A-Z])')
                 names_to = 'EWR_stat',
                 values_to = 'value') %>%
    tidyr::separate(EWR_stat, into = c('ewr_code', 'stat'), sep = '(_)(?!.*_)')

  # Do that again for the daysBetweenEvents
  gaugelongdays <- ewrdf %>%
    dplyr::select(tidyselect::all_of(c(refcols, daysbetweencols))) %>%
    # The cols would be nice to be a more general spec.
    tidyr::pivot_longer(cols = matches('([0-9])|([A-Z])', ignore.case = FALSE), # matches('([0-9])|([A-Z])')
                 names_to = 'EWR_stat',
                 values_to = 'value') %>%
    tidyr::separate(EWR_stat, into = c('ewr_code', 'stat'), sep = '(_)(?!.*_)')

  # I think give the stats their own columns, since they have different meanings
  # own cols? I think probably?
  gaugestats <- gaugelong %>%
    tidyr::pivot_wider(names_from = stat, values_from = value)

  gaugestatsdays <- gaugelongdays %>%
    tidyr::pivot_wider(names_from = stat, values_from = value)

  allgaugestats <- dplyr::left_join(gaugestats, gaugestatsdays, by = c(names(ewrdf)[refcols], 'ewr_code'))

  allgaugestats <- allgaugestats %>%
    tidyr::separate(ewr_code, into = c("ewr_code", "ewr_code_timing"), sep = "_", remove = FALSE)

  return(allgaugestats)


}

