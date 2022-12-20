#' Pair EWR indicators to environmental objectives
#'
#' Bespoke function to clean the EWR table and LTWP table, extract environmental objectives, and pair them. Will need to change or at least extensively retest if those datasets change
#'
#' @param ewrpath path to the EWR database. Defaults to the online version also hit by the EWR tool
#' @param objtablepath path to the LTWP table
#' @param returnformat one of "long" or "wide", controls the shape of the output.
#'   * `long` (the default): columns for EWR indicators and environmental objectives, with each combination existing in the dataset as a row
#'   * `wide`: rows of EWR indicators, columns of environmental objectives, with T/F indicating matching
#' @param saveout one of FALSE, 'r', or 'csv', controls saving and type.
#'   * `FALSE`: (the default): Does not save anything
#'   * `'r'`: saves an rdata file, used primarily for building the data directory in a package
#'   * `'csv'`: saves a csv with the name `outdir/savename_YearMonthDayHourMinute.csv`
#' @param outdir path to the directory to save into. not needed if `saveout` is `FALSE`
#' @param savename character for the filename to save. the date and time gets appended to avoid overwriting
#'
#' @return A `tibble` with columns for `WatReqID`, `PlanningUnitID`, `gauge`, `ewr_code`, `ewr_code_timing`, `LTWPShortName`, and `env_obj` (if `returnformat == 'long'`) or columns for each env obj (if `returnformat == 'wide'`)
#' @export
#'
#' @examples
#' # I am skipping examples because this is such a specific use.
#'
clean_ewr_obj <- function(ewrpath = 'https://az3mdbastg001.blob.core.windows.net/mdba-public-data/NSWEWR_LIVE.csv',
                          objtablepath,
                          returnformat = 'long',
                          saveout = FALSE,
                          outdir, savename) {

  # Water requirement descriptions (same as hit by EWR tool)
  # Only read in some cols and not the whole csv. we actually only need watreqid
  # and code here and could get the rest with joins later, but let's at least be
  # clear about the location

  # The names keep changing. Figure out the current name of WatReqID
  wri <- readr::read_csv(ewrpath, n_max = 0)
  wriname <- names(wri)[grepl('^WatReq', names(wri))]

  # Now we need to build a list. This is stupidly complicated
  collist <- list(wri = 'c',
                  PlanningUnitID = 'c',
                  LTWPShortName = 'c',
                  gauge = 'c',
                  code = 'c')

  names(collist)[1] <- wriname

  inputewr <- readr::read_csv(ewrpath,
                              col_types = collist) %>%
    dplyr::select(tidyselect::all_of(names(collist))) %>%
    dplyr::rename(ewr_code = code) %>%
    dplyr::rename_with(~'WatReqID', contains("WatReqID")) %>%
    dplyr::rename_with(~stringr::str_remove_all(., pattern = "/"), everything()) %>%
    dplyr::rename_with(~stringr::str_replace_all(., pattern = " ", replacement = '_'), everything()) %>%
    dplyr::distinct()

  # data from MDBA about objectives (scraped by mdba to produce the above, but
  # here we want the objectives, which are very messy)
  ewr2obj <- readr::read_csv(objtablepath, col_types = readr::cols()) %>%
    dplyr::rename_with(~'WatReqID', contains("WatReqID")) %>%
    # read_csv fixes the weird characters, but as a double-check,
    dplyr::mutate(dplyr::across(tidyselect::where(is.character),
                         ~stringr::str_replace_all(., pattern = "â€", replacement = "-"))) %>%
    # another special dash
    dplyr::mutate(dplyr::across(tidyselect::where(is.character),
                         ~stringr::str_replace_all(., pattern = "–", replacement = "-")))


  # Extract the Objectives from the big table

  # Some notes from testing
  # This is fragile to a couple things that don't currently appear in the data
  # numberletter-number, e.g. NF3a-8? stringr::str_which(objs, '[a-z]-') multi-dashes?
  # stringr::str_which(objs, "[0-9]-[0-9],")
  # Assumes only objectives prefixed by EF, NF, NV, OS, or WB. I had a version
  # that was [A-Z]+ and one that was [A-Z]{1,2}, but they pick up the EWRS too.

  # The main regex is first line, then cleanup (mostly to create the CODEnumber
  # for each one cleanly). This returns a list of
  # length(nrow(ewr2obj)) with all objectives in each row of
  # the big table
  objs <- stringr::str_extract_all(ewr2obj$Rationale,
                          pattern = '(EF|NF|NV|OS|WB) *([0-9][a-z]* *-* *((, *[a-z]*)* *)*)+') %>%
    # Remove all the spaces
    purrr::map(~stringr::str_remove_all(., pattern = ' ')) %>%
    # Some of the dashes are at the end of a list indicating moving to the next
    # objective, not a set of numbers
    purrr::map(~stringr::str_remove_all(., pattern = '-$')) %>%
    # Expand x-y style number ranges to a comma-separated char of all numbers
    purrr::map(~dash2comma(.)) %>%
    # deal with things like 4a,b or even more complex like 2,3,4,a,c,5,6d,f,8
    purrr::map(~lettercomma(.)) %>%
    # Replace commas with the Objective code, thereby gluing the objective code to each number
    purrr::map(~code2nums(.))

  # Glue the objective lists in to each of the water requirements as a list-column
  ewr2obj <- ewr2obj %>%
    tibble::as_tibble() %>% # not needed if read_csv
    dplyr::select(WatReqID, PlanningUnitID)%>%
    tibble::tibble(env_obj = objs) %>%
    # Attach the EWR codes, gauge numbers, and planning units
    dplyr::left_join(inputewr, by = c('WatReqID', 'PlanningUnitID')) %>%
    # unnest to duplicate Water requirement rows to all matching Objectives
    tidyr::unnest(cols = env_obj) %>%
    # arrange cols in a better order
    dplyr::select(WatReqID, PlanningUnitID, LTWPShortName, gauge, ewr_code, env_obj)

  # final cleanup of weird characters if we didn't catch them above
  # The toascii throws a warning for every nonstandard character. that's the whole point, so suppress them
  suppressWarnings(ewr2obj <- ewr2obj %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringi::stri_enc_toascii(.))) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringr::str_replace_all(.,'\032', '-'))) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringr::str_replace_all(.,'-$', ''))) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringr::str_squish(.))))

  # separate the ewr_code into main code and modifier
  suppressWarnings(ewr2obj <- ewr2obj %>%
    tidyr::separate(ewr_code, into = c("ewr_code", "ewr_code_timing"), sep = "_", remove = FALSE))

  if (returnformat == 'wide') {
    # make a wide version, with logical columns for matches
    # Not actually sure whether we want to keep this or deprecate, but it matches Georgia's format
    ewr2obj <- ewr2obj %>%
      dplyr::mutate(match = TRUE) %>%
      # fill unmatched with FALSE, otherwise it uses NA
      tidyr::pivot_wider(names_from = env_obj, values_from = match,
                  values_fill = FALSE) %>%
      # prettier col organise
      dplyr::select(WatReqID, PlanningUnitID, gauge, ewr_code, ewr_code_timing, everything())
  }


  # save
  if (saveout == 'r') {

    # Rdata for package structure
    saveRDS(ewr2obj, file = file.path(outdir, 'ewr2obj.rds'))

  } else if (saveout == 'csv') {

    # csv for other
    readr::write_csv(ewr2obj,
              file.path(outdir,
                        paste0(savename,
                               format(Sys.time(),
                                      "%Y%m%d%H%M"),
                               ".csv")))
  }

  return(ewr2obj)

}


# HELPERS -----------------------------------------------------------------



# Expand a dash into a numeric vector
expanddash <- function(x) {

  numx <- x %>%
    stringr::str_remove_all('[A-Z]') %>%
    stringr::str_extract_all('[0-9]+') %>%
    unlist() %>%
    as.numeric()

  numx <- seq(from = numx[1], to = numx[2])
  return(numx)
}

# Turn a dash into values separated by commas
dash2comma <- function(x) {

  # NA cause problems, and most things that have no entries just are
  # character(0), not NA. Can't check for NA if length 0 though
  if ((length(x) > 0) & all(is.na(x))) {
    return(character(0))
  }

  if((length(x) == 0) | any(!stringr::str_detect(x, pattern = '-'))) {
    return(x)
  }

  for (i in stringr::str_which(x, '-')) {
    numx <- expanddash(x[i])
    charx <- stringr::str_extract(x[i], '[A-Z][A-Z]')
    strx <- stringr::str_c(numx, collapse = ',')
    strx <- stringr::str_c(charx, strx)

    x[i] <- strx

  }

  return(x)
}

# Deal with things like 4a,b,d
lettercomma <- function(x) {
  otherbits <- stringr::str_remove_all(x, '[0-9][a-z](,[a-z])+')

  targets <- stringr::str_extract_all(x, '[0-9][a-z](,[a-z])+')

  num <- stringr::str_extract_all(unlist(targets), '[0-9]')

  let <- stringr::str_extract_all(unlist(targets), '[a-z]')

  repaired <- purrr::map2(num, let, stringr::str_c) %>%
    unlist() %>%
    stringr::str_flatten(',')

  putback <- stringr::str_c(otherbits, repaired, sep = ',') %>%
    stringr::str_replace_all(',{2,}', ',') %>%
    stringr::str_remove(',$')
  putback
}


# Glue the code to numbers

code2nums <- function(x) {
  objcode <- stringr::str_extract_all(x, '[A-Z]{2}')
  objnum <- stringr::str_extract_all(x, '[0-9]+[a-z]*')

  # outcat <- list()
  # for (i in 1:length(objcode)) {
  #   outcat[[i]] <- stringr::str_c(objcode[[i]], objnum[[i]])
  # }

  outvec <- purrr::map2(objcode, objnum, stringr::str_c) %>%
    unlist() %>%
    unique() # Some have the same objectives listed a few times

}
