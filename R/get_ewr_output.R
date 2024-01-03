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
get_ewr_output <- function(dir, type = 'achievement', year_roll = 'best',
                           gaugefilter = NULL, scenariofilter = NULL, add_max = TRUE) {

  if (type != 'achievement') {
    outdf <- get_any_ewr_output(dir, type = type, gaugefilter = gaugefilter, scenariofilter = scenariofilter)
  }

  if (type == "achievement") {
    yeardat <- get_any_ewr_output(dir, type = "yearly", gaugefilter = gaugefilter, scenariofilter = scenariofilter)
    sumdat <- get_any_ewr_output(dir, type = "summary", gaugefilter = gaugefilter, scenariofilter = scenariofilter)

    if (year_roll == "best") {
      year_roll <- ifelse(length(unique(yeardat$year)) >= 10, 10, 1)
    } else {
      year_roll <- year_roll
    }

    # assess achievement
    outdf <- assess_ewr_achievement(yeardat, sumdat, year_roll = year_roll)
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
      temp <- readr::read_csv(i, col_types = readr::cols())
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
    dplyr::mutate(dplyr::across(tidyselect::where(is.logical), as.numeric))

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
  # Gauges should be characters
  ewrdf$gauge <- as.character(ewrdf$gauge)

  names(ewrdf) <- nameclean(names(ewrdf))

  ewrdf <- ewrdf |>
    separate_ewr_codes()

  return(ewrdf)
}


#' Parser for EWR codes into the main code and the extra bits (called 'timing' for some reason)
#'
#' I want something that can be used against the whole ewr
# table and causal networks to keep them matched and standardize the parsing.
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
  df$ewr_code_timing <- extrapart

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
    stringr::str_replace_all(pattern = "-", replacement = "")

  # sometimes the ewr names are ewr_code and sometimes just ewr
  cleannames[cleannames == "ewr" | cleannames == "Code" | cleannames == "ewrCode"] <- "ewr_code"
  # the planning unit names (and IDs) keep getting changed and dropped, so they might be a few different things.
  cleannames[cleannames == "pu" | cleannames == "PlanningUnitName" | cleannames == "planning_unit"] <- "planning_unit_name"

  return(cleannames)
}



#' EWR logic test on incoming Annual EWRs
#'
#' This is the pass/fail test of whether the criteria (frequency and timing) of EWRs are met or not
#' Using the Minimum long term average (LTA) target frequencies (Termed Target frequency in summarydf) as suggested in the LTWPs
#' Includes inverse result for cease to flows (CF) and two frequency checks for longterm data (greater than 20 years)
#'
#' @param annualdf incoming tibble of EWRs after read-in
#' @param year_roll specific number of years to check assessment for
#' @param summarydf incoming tibble of  EWRs after read-in - contains the Target Frequency column
#'
#' @return tibble reformatted and cleaned for ongoing analysis
#' @export
#' @examples
assess_ewr_achievement <- function(annualdf, summarydf, year_roll = ifelse(nrow(annualdf) >= 10, 10, 1)) {
  # GET Target frequencies from NSWEWR (could also use NSWEWR from EWR tool?)
  Target_frequencies <- summarydf |>
    dplyr::select(planning_unit_name, gauge, ewr_code, ewr_code_timing, target_frequency) |>
    dplyr::group_by(planning_unit_name, gauge, ewr_code, ewr_code_timing, target_frequency) |>
    dplyr::distinct() |>
    dplyr::ungroup()

  # Join target frequencies to annualdf
  annualdf <- dplyr::left_join(annualdf, Target_frequencies,
    by = dplyr::join_by(ewr_code, ewr_code_timing, gauge, planning_unit_name),
    relationship = "many-to-many"
  )

  # Frequency checks (ACHIEVEMENT test)
  if (year_roll > 1) {
    # Specific rolling time frame
    # calculate number of event years, frequency, and EWR pass/fail at defined (year_roll) year rolling time frames.
    # cease to flows are the inverse of success.
    annualdf <- annualdf |>
      dplyr::group_by(scenario, planning_unit_name, gauge, ewr_code, ewr_code_timing) |>
      dplyr::arrange(scenario, planning_unit_name, gauge, ewr_code, ewr_code_timing, year) |>
      # defined n years
      dplyr::mutate(eventyears_per_n_years = purrr::reduce(purrr::map(1:(year_roll - 1), ~ lag(event_years, ., order_by = year)), `+`) + event_years) |>
      dplyr::mutate(frequency_per_n_years = (eventyears_per_n_years / year_roll) * 100) |>
      dplyr::mutate(frequency_check_n_years = ifelse(grepl("CF", ewr_code), frequency_per_n_years <= target_frequency,
        frequency_per_n_years >= target_frequency
      )) |>
      dplyr::ungroup()

    # ACHIEVEMENT test
    EWR_results <- annualdf |>
      dplyr::group_by(ewr_code, ewr_code_timing, gauge, scenario, planning_unit_name) |>
      dplyr::filter(!is.na(frequency_check_n_years)) |>
      dplyr::summarise(ewr_achieved = sum(frequency_check_n_years) == dplyr::n()) |> # do all pass?
      dplyr::mutate(ewr_achieved_timeframe = year_roll) |>
      dplyr::ungroup()
  } else if (year_roll <= 1) {
    # Pre-defined time frames
    # calculate number of event years, frequency, and EWR pass/fail at 10, 20 and all year rolling time frames.
    # cease to flows are the inverse of success.
    annualdf <- annualdf |>
      dplyr::group_by(scenario, planning_unit_name, gauge, ewr_code, ewr_code_timing) |>
      dplyr::arrange(scenario, planning_unit_name, gauge, ewr_code, ewr_code_timing, year) |>
      # 10 years
      dplyr::mutate(eventyears_per_10_years = purrr::reduce(purrr::map(1:(10 - 1), ~ lag(event_years, ., order_by = year)), `+`) + event_years) |>
      dplyr::mutate(frequency_per_10_years = (eventyears_per_10_years / 10) * 100) |>
      dplyr::mutate(frequency_check_10_years = ifelse(grepl("CF", ewr_code), frequency_per_10_years <= target_frequency,
        frequency_per_10_years >= target_frequency
      )) |>
      # 20 years
      dplyr::mutate(eventyears_per_20_years = purrr::reduce(purrr::map(1:(20 - 1), ~ lag(event_years, ., order_by = year)), `+`) + event_years) |>
      dplyr::mutate(frequency_per_20_years = (eventyears_per_20_years / 20) * 100) |>
      dplyr::mutate(frequency_check_20_years = ifelse(grepl("CF", ewr_code), frequency_per_20_years <= target_frequency,
        frequency_per_20_years >= target_frequency
      )) |>
      # All years
      dplyr::mutate(eventyears_per_all_years = sum(event_years)) |>
      dplyr::mutate(frequency_per_all_years = (sum(event_years) / dplyr::n()) * 100) |>
      dplyr::mutate(frequency_check_all_years = ifelse(grepl("CF", ewr_code), frequency_per_all_years <= target_frequency,
        frequency_per_all_years >= target_frequency
      )) |>
      dplyr::ungroup()

    nYdata <- length(unique(annualdf$year))

    # ACHIEVEMENT test
    if (nYdata < 10) {
      # less than 10 years data (uses all years as time frame window)
      EWR_results <- annualdf |>
        dplyr::group_by(ewr_code, ewr_code_timing, gauge, scenario, planning_unit_name) |>
        dplyr::summarise(ewr_achieved = sum(frequency_check_all_years) == dplyr::n()) |> # do all pass?
        dplyr::mutate(ewr_achieved_timeframe = nYdata) |>
        dplyr::ungroup()
    } else if (nYdata >= 10 & nYdata < 20) {
      # less than 20 years data (uses 10 year rolling time frame window)
      EWR_results <- annualdf |>
        dplyr::group_by(ewr_code, ewr_code_timing, gauge, scenario, planning_unit_name) |>
        dplyr::filter(!is.na(frequency_check_10_years)) |>
        dplyr::summarise(ewr_achieved = sum(frequency_check_10_years) == dplyr::n()) |> # do all pass?
        dplyr::mutate(ewr_achieved_timeframe = 10) |>
        dplyr::ungroup()
    } else if (nYdata >= 20) {
      # 20 or more years data (uses 10 and 20 year rolling time frame windows depending on EWR)
      EWR_results <- annualdf |>
        dplyr::group_by(ewr_code, ewr_code_timing, gauge, scenario, planning_unit_name) |>
        dplyr::mutate(
          frequency_check_10and20_years = ifelse(target_frequency < 10 | target_frequency > 90, frequency_check_20_years, frequency_check_10_years),
          ewr_achieved_timeframe = ifelse(target_frequency < 10 | target_frequency > 90, 20, 10)
        ) |>
        dplyr::filter(!is.na(frequency_check_10and20_years)) |>
        dplyr::summarise(
          ewr_achieved = sum(frequency_check_10and20_years) == dplyr::n(), # do all pass?
          ewr_achieved_timeframe = unique(ewr_achieved_timeframe)
        ) |>
        dplyr::ungroup()
    }
  }

  # change the logical to numeric to maintain generality with later functions
  EWR_results$ewr_achieved <- as.numeric(EWR_results$ewr_achieved)

  return(EWR_results)
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
    dplyr::filter(scenario == unique(outdf$scenario)[1])|>
    dplyr::mutate(scenario = "MAX",
           ewr_achieved = 1)|>
    dplyr::select(scenario, gauge, planning_unit_name, ewr_achieved, ewr_code, ewr_code_timing)
  outdf <- dplyr::bind_rows(outdf, MAX_scenario)
  return(outdf)
}

