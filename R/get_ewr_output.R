#' Make EWR results with achievement for ongoing use
#'
#' @param dir path to directory with the EWR output for all gauges and scenarios
#' @param year_roll character 'best' or number, specific number of years to check assessment for. 'Best' uses 10-year windows if possible. 1 uses the NSW method.
#' @param type character, one of:
#'  * 'achievement' (default)- gets the yearly summarised to the period with calculated EWR achievement
#'  * 'summary',
#'  * 'yearly',
#'  * 'all_events',
#'  * 'all_successful_events',
#'  * 'all_interEvents', # Does not work with current EWR tool
#'  * 'all_successful_interEvents'
#' @param gaugefilter subset of gauges, default NULL
#' @param scenariofilter subset of scenarios, default NULL
#' @param add_max logical, default TRUE. Add a 'MAX' scenario that passes all EWRs, usable as a reference
#'
#' @return a tibble with ewr_achieved
#' @export
#'
#' @examples
get_ewr_output <- function(dir, type = "achievement", year_roll = "best",
                           gaugefilter = NULL, scenariofilter = NULL, add_max = TRUE) {
  if (type != "achievement") {
    outdf <- get_any_ewr_output(dir, type = type,
                                gaugefilter = gaugefilter,
                                scenariofilter = scenariofilter)
  }

  if (type == "achievement") {
    yeardat <- get_any_ewr_output(dir, type = "yearly",
                                  gaugefilter = gaugefilter,
                                  scenariofilter = scenariofilter)

    yeardat <- clean_yearly(yeardat)

    if (year_roll == "best") {
      year_roll <- ifelse(length(unique(yeardat$year)) >= 10, 10, 1)
    } else {
      year_roll <- year_roll
    }

    # assess achievement
    outdf <- assess_ewr_achievement(yeardat, year_roll = year_roll)
    # add max
    if (add_max == TRUE) {
      outdf <- bind_max(outdf)
    }
  }

  return(outdf)
}





#' Get the outputs from the EWR and clean them up
#'
#' *NOTE* this expects output from the EWRs to be processed through the toolkit
#' functions for cleaning and saving immediately after creation, which may
#' change now that `py-ewr` has changed some things.
#'
#' @param dir path to directory with the EWR output for all gauges and scenarios
#' @param type character, one of
#'  * 'summary',
#'  * 'yearly',
#'  * 'all_events',
#'  * 'all_successful_events',
#'  * 'all_interEvents', # Does not work with current EWR tool
#'  * 'all_successful_interEvents'
#' @param gaugefilter character vector of gauge numbers to include. Default `NULL` includes all
#' @param scenariofilter character vector of scenario names to include. Default `NULL` includes all
#'
#' @return a tibble of EWR results if `type == "annual"` or `type == "summary"`. A named list of those tibbles if `type == "both"`
#' @export
#'
#' @examples
get_any_ewr_output <- function(dir, type,
                               gaugefilter = NULL, scenariofilter = NULL) {
  if (is.character(dir)) {
    # assumes files are csvs.
    gaugefiles <- list.files(dir,
      pattern = ".csv",
      full.names = TRUE, recursive = TRUE
    )

    # only get the relevant type of ewr output
    if (type != "everything") {
      relevantfiles <- gaugefiles[stringr::str_which(gaugefiles, pattern = type)]
    }
    if (type == "everything") {
      relevantfiles <- gaugefiles
    }


    # cut to requested gauges or scenarios
    if (!is.null(gaugefilter)) {
      relevantfiles <- relevantfiles[stringr::str_which(relevantfiles,
        pattern = stringr::str_flatten(gaugefilter, collapse = "|")
      )]
    }

    if (!is.null(scenariofilter)) {
      relevantfiles <- relevantfiles[stringr::str_which(relevantfiles,
        pattern = stringr::str_flatten(scenariofilter, collapse = "|")
      )]
    }

    # read into one df
    ewrdata <- foreach::foreach(
      i = relevantfiles,
      .combine = dplyr::bind_rows
    ) %do% {
      # gauge needs to be character, but often looks numeric
      temp <- readr::read_csv(i, col_types = readr::cols(
        scenario = readr::col_character(),
        gauge = readr::col_character()
      ))
    }
  } else if (is.list(dir)) {
    ewrdata <- dir[[type]]

    if (!is.null(gaugefilter)) {
      ewrdata <- ewrdata |> dplyr::filter(gauge %in% gaugefilter)
    }

    if (!is.null(scenariofilter)) {
      ewrdata <- ewrdata |> dplyr::filter(scenario %in% scenariofilter)
    }
  }

  # There are sometimes wholly-blank columns that are read as NA, but should be
  # numeric. We can't pre-set them with readr because they may be in different
  # places for different gauges
  ewrdata <- ewrdata |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.logical), as.numeric)) |>
    dplyr::mutate(
      gauge = as.character(gauge),
      scenario = as.character(scenario)
    ) # belt and braces- this should never be anything else at this point

  ewrdata <- suppressWarnings(cleanewrs(ewrdata))


  return(ewrdata)
}


# Clean up the EWRs


#' Clean up and standardise names and column types
#'
#' The new format of the EWR output makes this much lighter than previously.
#'
#' @param ewrdf ewr dataframe of any type
#'
#' @return a tibble of EWR outputs with cleaned up names, separated ewr_code_timing, and gauges as characters
#' @export
#'
#' @examples
cleanewrs <- function(ewrdf) {

  names(ewrdf) <- nameclean(names(ewrdf))

  # Gauges should be characters
  ewrdf$gauge <- as.character(ewrdf$gauge)

  ewrdf <- ewrdf |>
    separate_ewr_codes()

  return(ewrdf)
}

#' Additional cleanup specific to yearly EWR outputs.
#'
#' @param annualdf the result of [get_any_ewr_ewr_output()] with `type = 'yearly'`
#'
#' @return
#' @export
clean_yearly <- function(annualdf) {

  # Dates break on the water/financial year.
  # and represent the start of the period- macq historic scenario: data starts - 1889-01-01 and the first EWR result is for 1888. confirms dates are the beginning, 2014 = 2014-2015
  annualdf$date <- as.Date(paste0(as.character(annualdf$year), '-07-01'))

  # The first year is untrustworthy due to how the EWR does its calculations. Make the returned values NA

  ewr_calc_cols <- c('event_years', 'num_achieved', 'num_events', 'num_events_all', 'event_length', 'event_length_achieved', 'total_event_days', 'total_event_days_achieved', 'max_event_days', 'max_rolling_events', 'max_rolling_achievement', 'missing_days', 'total_possible_days', 'rolling_max_inter_event', 'rolling_max_inter_event_achieved')

  annualdf <- annualdf |>
    dplyr::mutate(dplyr::across(tidyselect::all_of(ewr_calc_cols),
                                \(x) ifelse(date == min(date, na.rm = TRUE), NA, x)))



}


#' Cleans the EWR table to use for assessing target requirements by the outputs
#'
#' @return clean tibble suitable for joining to outputs
#' @export
#'
clean_ewr_requirements <- function() {
  # Get the target frequencies and interevent durations
  ewr_requirements <- get_ewr_table() |>
    tibble::tibble() |>
    cleanewrs()

  ewr_requirements <- ewr_requirements |>
    dplyr::select(planning_unit_name, gauge,
                  tidyselect::contains("ewr_code"),
                  tidyselect::starts_with('target_frequency'),
                  tidyselect::contains('interevent'))

  # Target frequencies come in here as character.
  ewr_requirements <- ewr_requirements |>
    dplyr::mutate(across(tidyselect::starts_with('target'), as.numeric),
                  across(tidyselect::contains("interevent"), as.numeric)) |>
    # Make the NA target frequencies 0, since we can't assess them otherwise.
    # They only appear for CF_a EWRs, where the corresponding CF_b is 5 and CF_c
    # is 50
    dplyr::mutate(across(tidyselect::starts_with('target'), \(x) ifelse(is.na(x), 0, x))) |>
    dplyr::mutate(across(tidyselect::contains('interevent'), \(x) ifelse(is.na(x), 0, x)))

  return(ewr_requirements)
}


#' Parser for EWR codes into the main code and the extra bits (called 'timing' for some reason)
#'
#' This allows keeping the whole ewr table, the EWR outputs, and causal networks matched and standardized parsing.
#'
#' @param df a dataframe with an ewr_code column with raw ewr names (e.g. EWR outputs, causal mappings)
#'
#' @return a dataframe with a clean ewr_code column and an ewr_code_timing column with the extra stuff
#' @export
#'
#' @examples
separate_ewr_codes <- function(df) {
  # We need a consistent way to parse ewr codes from EWR tool and causal
  # networks. This isnt' perfect, but it's much better than before
  basestring <- df$ewr_code |>
    # ewr returns the / as _
    stringr::str_replace("/", "_") |>
    # change the _ to - so we can split on _
    stringr::str_replace("OB_W", "OB-W")

  # Get a clean main EWR code (as best I can)
  ewrpart <- basestring |>
    # get the bit before the first _
    stringr::str_extract("^[^_]+") |>
    # some have the a,b,c, attached instead of separated
    stringr::str_remove("[a-z]$")

  # Get the leftovers. Typically _P, _S, _a, etc, but sometimes weirder
  extrapart <- basestring |>
    # remove the main ewr string from the full string
    stringr::str_remove(ewrpart) |>
    # remove leading _
    stringr::str_remove("^_")

  # put back on the df
  df$ewr_code <- ewrpart
  # The code_timing needs to actually be unique, i.e. _a shouldn't match to everything with an _a, but to the EWR code with _a.
  df$ewr_code_timing <- paste0(ewrpart, '_', extrapart) |>
    gsub('_$', '', x = _)

  return(df)
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

  summarydf <- summarydf |>
    dplyr::mutate(ewr_achieved = target_frequency <= frequency)
}



#
#' Helper to sort the names
#'
#' @param charvec vector of names
#'
#' @return a character vector

nameclean <- function(charvec) {
  cleannames <- charvec |>
    stringr::str_replace_all("([A-Z])", "_\\1") |>
    tolower() |>
    stringr::str_replace_all(pattern = " ", replacement = "_") |>
    stringr::str_replace_all(pattern = "-", replacement = "") |>
    stringr::str_replace_all(pattern ='^_', replacement =  '')

  # sometimes the ewr names are ewr_code and sometimes just ewr
  cleannames[cleannames == "ewr" | cleannames == "Code" | cleannames == "code" |cleannames == "ewrCode"] <- "ewr_code"
  # the planning unit names (and IDs) keep getting changed and dropped, so they might be a few different things.
  cleannames[cleannames == "pu" | cleannames == "PlanningUnitName" | cleannames == "planning_unit"] <- "planning_unit_name"

  return(cleannames)
}



#' EWR logic test on incoming Annual EWRs
#'
#' This is the pass/fail test of whether the criteria (frequency and timing) of EWRs are met or not
#' Using the Minimum long term average (LTA) target frequencies (Termed Target frequency in summarydf) as suggested in the LTWPs
#' Includes inverse result for cease to flows (CF) in both the assessment and the event_years themselves
#'
#' @param annualdf incoming tibble of EWRs after read-in
#' @param year_roll specific number of years to check assessment for
#' @param summarydf incoming tibble of  EWRs after read-in - contains the Target Frequency column
#'
#' @return tibble reformatted and cleaned for ongoing analysis
#' @export
#' @examples
assess_ewr_achievement <- function(annualdf, year_roll = ifelse(nrow(annualdf) >= 10, 10, 1)) {

  # Need to get the ewr targets to check against
  ewr_requirements <- clean_ewr_requirements()

  # Join target frequencies to annualdf
  annualdf <- dplyr::left_join(annualdf, ewr_requirements,
    by = dplyr::join_by(ewr_code, ewr_code_timing, gauge, planning_unit_name),
    relationship = "many-to-many"
  )

  # FLIP EVENTS FOR CEASE-TO-FLOW
  # This makes a 1 a good thing, like all the others.
  # we also have to flip the target frequency; e.g. previously had a target that said we needed to have less than 80% ceases, we now need to have more than 20% not-ceases
  annualdf <- annualdf |>
    dplyr::mutate(event_years = ifelse(grepl('^CF', ewr_code), 1-event_years, event_years),
                  target_frequency = ifelse(grepl('^CF', ewr_code), 100-target_frequency, target_frequency))

  # Frequency checks (ACHIEVEMENT test)

    # calculate number of event years, frequency, and EWR pass/fail at defined (year_roll) year rolling time frames.
    # cease to flows are the inverse of success.
    annualdf <- annualdf |>
      # dplyr::group_by(scenario, planning_unit_name, gauge, ewr_code, ewr_code_timing) |>
      dplyr::arrange(scenario, planning_unit_name, gauge, ewr_code, ewr_code_timing, year) |>
      dplyr::mutate(frequency_occurred = roll_frequency(event_years, year_roll),
                    interevent_occurred = roll_interevent(event_years, year_roll),
                    .by = c(scenario, planning_unit_name, gauge, ewr_code, ewr_code_timing)) |>
      dplyr::mutate(ewr_achieved = frequency_occurred >= target_frequency,
                    interevent_achieved = interevent_occurred <= max_interevent,
                    .by = c(scenario, planning_unit_name, gauge, ewr_code, ewr_code_timing))

  # change the logical to numeric to maintain generality with later functions
  annualdf$ewr_achieved <- as.numeric(annualdf$ewr_achieved)
  annualdf$interevent_achieved <- as.numeric(annualdf$interevent_achieved)

  annualdf <- annualdf |>
    dplyr::select(scenario, year, date, gauge, planning_unit_name,
                  ewr_code, ewr_code_timing, event_years, ewr_achieved, interevent_achieved)

  return(annualdf)
}



#' Helper to get the frequency of occurrence without clogging up the mutates
#'
#'
#' @param x vector to calculate rolling frequencies for
#' @param year_roll window size for the roll
#' @param pad_initial Allow calculating values in the first years < year_roll; roll the year_roll if possible, otherwise as much as possible. Note that this makes the 1:year_roll entries less smoothed.
#'
#' @return
#' @export
#'
#' @examples
roll_frequency <- function(x, year_roll, pad_initial = FALSE, na.rm = FALSE) {

  if (pad_initial) {
    x <- c(rep(NA, year_roll), x)
  }

  if (year_roll == 1) {
    sumvec <- x
  } else {
    # RcppRoll *should* be faster (and is certainly cleaner), but in practice
    # isn't much faster and rlang::is_installed is really slow, so better to not
    # take the dependency
    # if (rlang::is_installed('RcppRoll')) {
    #   sumvec <- RcppRoll::roll_sum(x, n = year_roll, align = 'right', fill = NA, na.rm = TRUE)
    #  } else {
    # get a list of the values at each lag, make it a matrix with cols shifte by one, and sum across
    lagmat <- purrr::map(0:(year_roll - 1), \(y) dplyr::lag(x, y)) |>
      purrr::list_c() |>
      matrix(nrow = length(x))
    # we might want na.rm = TRUE to deal with intermediate NA and because we are
    # checking whether a frequency is greater than a value, and so the sum with
    # na.rm = TRUE gives us the *minimum* frequency for a sequence with NA,
    # which may stil be greater than the threshold. But that can cause fails if
    # the threshold is high, where we dont' really know and so values *should*
    # be NA. And it has the side effect of giving 0 if everythign is NA, and we
    # want to keep those NA.
    if (na.rm) {
      bothna <- rowSums(is.na(lagmat))
      sumvec <- rowSums(lagmat, na.rm = TRUE)
      sumvec[bothna == ncol(lagmat)] <- NA
    }
    if (!na.rm) {
      sumvec <- rowSums(lagmat)
    }


    # The way the rowsums works with na.rm = TRUE, we end up padding initial. Undo that if we don't want it
    if (!pad_initial) {
      sumvec[1:(year_roll-1)] <- NA
    }
  }


   # }

  freqvec <- (sumvec/year_roll)*100

  if (pad_initial) {
    # cut off that pre-padding
    freqvec <- freqvec[(year_roll+1):length(freqvec)]
  }

  return(freqvec)
}


#' Helper to get the rolling maximum interevent duration without clogging up the mutates
#'
#'
#' @param x vector to calculate rolling interevents for
#' @param year_roll window size for the roll
#' @param pad_initial Allow calculating values in the first years < year_roll; roll the year_roll if possible, otherwise as much as possible. Note that this makes the 1:year_roll entries less smoothed.
#'
#' @return
#' @export
#'
#' @examples
roll_interevent <- function(x, year_roll, pad_initial = FALSE) {
  if (pad_initial) {
    x <- c(rep(NA, year_roll), x)
  }

  if (year_roll == 1) {
    inters <- x
    inters[x == 0] <- 1
    inters[x > 0] <- 0
  } else {
    # RcppRoll *should* be faster (and is certainly cleaner), but in practice
    # isn't much faster and rlang::is_installed is really slow, so better to not
    # take the dependency
    # if (rlang::is_installed('RcppRoll')) {
    #   sumvec <- RcppRoll::roll_sum(x, n = year_roll, align = 'right', fill = NA, na.rm = TRUE)
    #  } else {
    # get a list of the values at each lag, make it a matrix with cols shifted by one, and sum across
    lagmat <- purrr::map(0:(year_roll - 1), \(y) dplyr::lag(x, y)) |>
      purrr::list_c() |>
      matrix(nrow = length(x))
    # get the interevents for each row (target year)
    inters <- apply(lagmat, MARGIN = 1, FUN = maxInterevent)
    # NA but no 0 becomes -Inf, make NA instead
    inters[is.infinite(inters)] <- NA

    # If there are zeros at the beginning, we get results. If we want to only return results for windows with full data, remove
    if (!pad_initial) {
      inters[1:(year_roll-1)] <- NA
    }
  }

  if (pad_initial) {
    # cut off that pre-padding
    inters <- inters[(year_roll+1):length(inters)]
  }

  return(inters)
}

#' Get maximum interevent duration of a vector
#'
#' Uses [base::rle()] to get the maximum length of 0s
#'
#' @param x numeric vector
#'
#' @return numeric scalar of the maximum run of 0s
#' @export
#'
maxInterevent <- function(x) {

  # Should be NA if all values are NA
  if (all(is.na(x))) {
    maxinter <- NA
  } else if (any(is.na(x)) & all(x == 1, na.rm = TRUE)) {
    # Should be NA if all values are 1 or NA (can't assess whether that NA was a 0 or 1)
    maxinter <- NA
  } else if (all(x == 1, na.rm = TRUE)) {
    # If there were all passing, interevent is 0
    maxinter <- 0
  } else {
    rlex <- rle(x)

    # if (all(is.na(rlex$lengths[rlex$values == 0]))) {
    #   a <- 1
    # }
    maxinter <- max(rlex$lengths[rlex$values == 0], na.rm = TRUE)
  }

  return(maxinter)
}

#' Add max scenario
#'
#' @param outdf #EWR summary output with pass fail results
#'
#' @return
#' @export
#'
#' @examples
bind_max <- function(outdf) {
  MAX_scenario <- outdf |>
    dplyr::select(gauge, planning_unit_name, ewr_code, ewr_code_timing) |>
    dplyr::distinct() |>
    dplyr::mutate(
      scenario = "MAX",
      ewr_achieved = 1
    ) |>
    dplyr::select(scenario, gauge, planning_unit_name, ewr_achieved, ewr_code, ewr_code_timing)
  outdf <- dplyr::bind_rows(outdf, MAX_scenario)
  return(outdf)
}
