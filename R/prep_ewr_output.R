#' Make EWR results with achievement for ongoing use
#'
#' @param dat EWR output dataframe (usually for all gauges and scenarios)
#' @param type as in [read_and_geo()], but with two special options:
#'  * 'achievement', calculates EWR achievement from 'yearly',
#'  * 'interevents', calculates some interevent values from all_interEvents or all_successful_interEvents
#'  For the EWR tool, the direct options are
#'  * 'summary',
#'  * 'yearly',
#'  * 'all_events',
#'  * 'all_successful_events',
#'  * 'all_interEvents',
#'  * 'all_successful_interEvents'
#' @param year_roll character 'best' or number, specific number of years to check assessment for. 'Best' uses 10-year windows if possible. 1 uses the NSW method.
#' @param gaugefilter subset of gauges, default NULL
#' @param scenariofilter subset of scenarios, default NULL
#' @param add_max logical, default TRUE only if `type = 'achievement'`. Add a 'MAX' scenario that passes all EWRs, usable as a reference
#'
#' @return a tibble with ewr_achieved
#' @export
#'

prep_ewr_output <- function(dat, type = "achievement", year_roll = "best",
                           gaugefilter = NULL, scenariofilter = NULL,
                           add_max = ifelse(type == 'achievement', TRUE, FALSE)) {

  # some cleanup that lets this get used on its own if necessary
  if ('eventYears' %in% names(dat)) {
    names(dat) <- nameclean(names(dat))
  }

  if (!inherits(dat, 'sf')) {
    dat <- join_to_geo(dat, HydroBOT::bom_basin_gauges)
  }
  # assorted cleanup
  dat <- cleanewrs(dat)

  if (!type %in%  c("achievement", "interevents")) {
    outdf <- dat
  }

  if (type == "achievement") {

    yeardat <- clean_ewr_yearly(dat)

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

  if (type == "interevents") {

    # assess interevent calculations
    outdf <- assess_ewr_interevents(dat)
    # add max
    if (add_max == TRUE) {
      rlang::abort('add_max does not make sense for interevents. use `add_max = FALSE`')
    }
  }

  return(outdf)
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

cleanewrs <- function(ewrdf) {

  # Gauges should be characters
  if ('gauge' %in% names(ewrdf)) {
    ewrdf$gauge <- as.character(ewrdf$gauge)
  }

  if ('ewr_code' %in% names(ewrdf)) {
    ewrdf <- ewrdf |>
      separate_ewr_codes()
  }

  return(ewrdf)
}

#' Additional cleanup specific to yearly EWR outputs.
#'
#' @param annualdf the result of [get_module_output()] for EWR data with `type = 'yearly'`
#'
#' @return tibble of the cleaned yearly EWR output
#' @export
clean_ewr_yearly <- function(annualdf) {

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
    tibble::tibble()

  names(ewr_requirements) <- nameclean(names(ewr_requirements))

  ewr_requirements <- cleanewrs(ewr_requirements)

  ewr_requirements <- ewr_requirements |>
    dplyr::select('state', 'SWSDLName', 'planning_unit_name', 'gauge',
                  tidyselect::contains("ewr_code"),
                  tidyselect::starts_with('target_frequency'),
                  tidyselect::contains('interevent'))

  # Target frequencies come in here as character.
  ewr_requirements <- ewr_requirements |>
    dplyr::mutate(dplyr::across(tidyselect::starts_with('target'), as.numeric),
                  dplyr::across(tidyselect::contains("interevent"), as.numeric)) |>
    # Make the NA target frequencies 0, since we can't assess them otherwise.
    # They only appear for CF_a EWRs, where the corresponding CF_b is 5 and CF_c
    # is 50
    dplyr::mutate(dplyr::across(tidyselect::starts_with('target'), \(x) ifelse(is.na(x), 0, x))) |>
    dplyr::mutate(dplyr::across(tidyselect::contains('interevent'), \(x) ifelse(is.na(x), 0, x)))

  return(ewr_requirements)
}


#' Parser for EWR codes into the main code and the extra bits (called 'timing' for some reason)
#'
#' This allows keeping the whole ewr table, the EWR outputs, and causal networks matched and standardized parsing.
#'
#' @param df a dataframe with an ewr_code column with raw ewr names (e.g. EWR outputs, causal mappings)
#'
#' @return a tibble with a clean ewr_code column and an ewr_code_timing column with the extra stuff
#' @export
#'

separate_ewr_codes <- function(df) {
  # We need a consistent way to parse ewr codes from EWR tool and causal
  # networks. This isnt' perfect, but it's much better than before
  basestring <- df$ewr_code |>
    # ewr returns the / as _
    stringr::str_replace("/", "_")

  # Get a clean main EWR code (as best I can)
  ewrpart <- basestring  |>
    # change the _ to - so we can split on _
    stringr::str_replace("OB_W", "OB-W") |>
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
  df$ewr_code_timing <- basestring |>
    gsub('_$', '', x = _)

  return(df)
}


#' EWR logic test on incoming Annual EWRs
#'
#' This is the pass/fail test of whether the criteria (frequency and timing) of EWRs are met or not
#' Using the Minimum long term average (LTA) target frequencies (Termed Target frequency in summarydf) as suggested in the LTWPs
#' Includes inverse result for cease to flows (CF) in both the assessment and the event_years themselves
#'
#' @param annualdf incoming tibble of EWRs after read-in
#' @param year_roll specific number of years to check assessment for
#'
#' @return tibble with columns `scenario`, `year`, `date`, `gauge`, `planning_unit_name`, `state`, `SWSDLName`, `ewr_code`, `ewr_code_timing`, `event_years`, `frequency_achieved`, `interevent_achieved`, `ewr_achieved`
#' @export

assess_ewr_achievement <- function(annualdf, year_roll = ifelse(nrow(annualdf) >= 10, 10, 1)) {

  # Need to get the ewr targets to check against
  ewr_requirements <- clean_ewr_requirements()

  # Join target frequencies to annualdf
  annualdf <- dplyr::left_join(annualdf, ewr_requirements,
    by = c('ewr_code', 'ewr_code_timing', 'gauge',
           'planning_unit_name', 'state', 'SWSDLName'),
    relationship = "many-to-many"
  )

  # FLIP EVENTS FOR CEASE-TO-FLOW
  # This makes a 1 a good thing, like all the others.
  # we also have to flip the target frequency; e.g. previously had a target that said we needed to have less than 80% ceases, we now need to have more than 20% not-ceases
  annualdf <- annualdf |>
    dplyr::mutate(event_years = ifelse(grepl('^CF', .data$ewr_code),
                                       1-.data$event_years, .data$event_years),
                  target_frequency = ifelse(grepl('^CF', .data$ewr_code),
                                            100-.data$target_frequency, .data$target_frequency),
                  max_interevent = .data$max_interevent*365)

  # Frequency checks (ACHIEVEMENT test)

    # calculate number of event years, frequency, and EWR pass/fail at defined (year_roll) year rolling time frames.
    # cease to flows are the inverse of success.
    annualdf <- annualdf |>
      # dplyr::group_by(scenario, planning_unit_name, gauge, ewr_code, ewr_code_timing) |>
      dplyr::arrange(.data$scenario, .data$planning_unit_name,
                     .data$gauge, .data$ewr_code, .data$ewr_code_timing,
                     .data$year) |>
      dplyr::mutate(frequency_occurred = roll_frequency(.data$event_years, year_roll),
                    # the interevents are highly variable (and often sub-yearly), so rolling by a certian number of years and the dataframe provides a rolling_achievement anyway
                    # interevent_occurred = roll_interevent(.data$event_years, year_roll),
                    .by = c("scenario", "planning_unit_name", 'state', 'SWSDLName',
                            "gauge", "ewr_code", "ewr_code_timing")) |>
      # Some EWRs are missing target frequencies and max interevents. Treat
      # those as if those conditions don't exist (since they dont). Thus, if an
      # event happens with no target, just pass it, and if there are no
      # interevent conditions, pass the interevent
      dplyr::mutate(frequency_achieved = ifelse(is.na(.data$target_frequency), .data$event_years,
                                                .data$frequency_occurred >= .data$target_frequency),
                    # we could use interevent_achieved = .data$rolling_max_inter_event_achieved, but the following lets us specify the function later, and is what the EWR tool does internally
                    interevent_achieved = ifelse(is.na(.data$max_interevent), 1,
                                                 as.numeric(.data$rolling_max_inter_event <= .data$max_interevent)),
                    # both have to occur for the EWR to 'pass'
                    ewr_achieved = .data$frequency_achieved * .data$interevent_achieved,
                    .by = c("scenario", "planning_unit_name", 'state', 'SWSDLName',
                            "gauge", "ewr_code", "ewr_code_timing"))

  # change the logical to numeric to maintain generality with later functions
  annualdf$frequency_achieved <- as.numeric(annualdf$frequency_achieved)
  annualdf$interevent_achieved <- as.numeric(annualdf$interevent_achieved)

  annualdf <- annualdf |>
    dplyr::select('scenario', 'year', 'date', 'gauge',
                  'planning_unit_name', 'state', 'SWSDLName',
                  'ewr_code', 'ewr_code_timing',
                  'event_years', 'frequency_achieved',
                  'interevent_achieved', 'ewr_achieved')

  return(annualdf)
}

#' Some calculations needed for aggregation of interevent statistics
#'
#' @param interdf the all_interEvents or all_successful_interEvents EWR output
#'
#' @return tibble with columns `scenario`, `gauge`, `planning_unit_name`,
#'   `state`, `SWSDLName`, `ewr_code`, `start_date`, `inter_event_length`,
#'   `ewr_code_timing`, `max_interevent`, `exceedance_days` (realised - max),
#'   `interevent_ratio` (realised / max), `exceedance_ratio` (interevent_ratio -
#'   1), `exceedance` (binary; realised >= max), `exceedance_only` (days above
#'   max), `days_in_exceeding (all realised if max passed)`
#' @export
#'
assess_ewr_interevents <- function(interdf) {

  # Need to get the ewr targets to check against
  ewr_requirements <- clean_ewr_requirements()

  # Join target frequencies to interdf
  interdf <- dplyr::left_join(interdf, ewr_requirements,
                               by = c('ewr_code', 'ewr_code_timing', 'gauge',
                                      'planning_unit_name', 'state', 'SWSDLName'),
                               relationship = "many-to-many"
  )

  # max_interevent in the parameter sheet (and so here) is yearly, but the
  # interevents themselves are daily. EWR tool just *365, so I'll do the same

  interdf <- interdf |>
    dplyr::mutate(max_interevent = .data$max_interevent*365)

  # calculate exceedance as the number of days, a ratio, and binary
  # these are needed to implement the MDBA 'Interevent statistics' sheet.
  interdf <- interdf |>
    dplyr::mutate(
      # there will be negatives here
      exceedance_days = .data$inter_event_length - .data$max_interevent,
      interevent_ratio = .data$inter_event_length / .data$max_interevent,
      exceedance_ratio = .data$interevent_ratio - 1, # this is (interevent-max/max), ie how much exceedance is relative to the max.
      # Binary- this says hitting the max days is a failure. That's what the sheet says.
      exceedance = as.numeric(.data$exceedance_days >= 0),
      # this isn't strictly necessary, but simplifies life a bit to have a
      # version that is only the excesses.
      exceedance_only = .data$exceedance_days * .data$exceedance,
      # this is the total length in interevents that exceed (including pre-exceedance)
      days_in_exceeding = .data$inter_event_length * .data$exceedance
      #
    )

  # remove some unneeded cols
  interdf <- interdf |>
    # dropping end_date is potentially bad, but it simplifies temporal
    # aggregation and we can always get it back with start_date +
    # lubridate::days(inter_event_length)
    dplyr::select(-'end_date', -tidyselect::contains('frequency'))

  return(interdf)
}



#' Helper to get the frequency of occurrence without clogging up the mutates
#'
#'
#' @param x vector to calculate rolling frequencies for
#' @param year_roll window size for the roll
#' @param pad_initial Allow calculating values in the first years < year_roll; roll the year_roll if possible, otherwise as much as possible. Note that this makes the 1:year_roll entries less smoothed.
#' @param na.rm default FALSE, na action for the sums over lags- TRUE is tempting, but can cause fails for high thresholds when we don't actually know whether they're met, and so FALSE is more appropriate (keeps intermediates NA).
#' @return numeric vector
#' @keywords internal
#'

roll_frequency <- function(x, year_roll, pad_initial = FALSE, na.rm = FALSE) {

  if (pad_initial) {
    if (!na.rm) {
      rlang::abort(c('Using `pad_initial = TRUE` and `na.rm = FALSE` is not logical.',
                    'pad_initial requires estimating over subsets of the `year_roll` span, and so implies `na.rm = TRUE`.'))
    }
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
    # which may still be greater than the threshold. But that can cause fails if
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
#' @return numeric vector
#' @keywords internal
#'

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
#' @return tibble with a MAX scenario added
#' @keywords internal
bind_max <- function(outdf) {
  MAX_scenario <- outdf |>
    sf::st_drop_geometry() |>
    dplyr::select('gauge', 'planning_unit_name', 'state', 'SWSDLName', 'ewr_code', 'ewr_code_timing') |>
    dplyr::distinct() |>
    dplyr::mutate(
      scenario = "MAX",
      ewr_achieved = 1
    ) |>
    dplyr::select('scenario', 'gauge', 'planning_unit_name', 'state', 'SWSDLName',
                  'ewr_achieved', 'ewr_code', 'ewr_code_timing') |>
    join_to_geo(bom_basin_gauges)

  outdf <- dplyr::bind_rows(outdf, MAX_scenario)
  return(outdf)
}
