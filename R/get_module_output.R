#' Get the outputs from modules and do minor cleaning
#'
#' *NOTE* Individual modules may need further bespoke processing, see [prep_ewr_output()].
#'
#' @param dir path to directory with the EWR output for all gauges and scenarios
#' @param type character, a grep for the files to choose. Two special cases:
#'  * 'achievement', calculates EWR achievement from 'yearly',
#'  * 'everything', gets all files
#'  For the EWR tool, the direct options are
#'  * 'summary',
#'  * 'yearly',
#'  * 'all_events',
#'  * 'all_successful_events',
#'  * 'all_interEvents',
#'  * 'all_successful_interEvents'
#' @param gaugefilter character vector of gauge numbers to include. Default `NULL` includes all
#' @param scenariofilter character vector of scenario names to include. Default `NULL` includes all
#'
#' @return a tibble of module results
#' @export
#'

get_module_output <- function(dir, type,
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


    # cut to requested gauges or scenarios if possible based on filenames
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
    # need to know if there's a 'gauge' col
    isgauge <- 'gauge' %in% names(readr::spec_csv(relevantfiles[1])$cols)


    # make CHECK happy
    i <- NULL
    module_data <- foreach::foreach(
      i = relevantfiles,
      .combine = dplyr::bind_rows
    ) %do% {

      if (isgauge) {
        # gauge needs to be character, but often looks numeric
        temp <- readr::read_csv(i, col_types = readr::cols(
          scenario = readr::col_character(),
          gauge = readr::col_character()
        ))
      } else {
        # gauge needs to be character, but often looks numeric
        temp <- readr::read_csv(i, col_types = readr::cols(
          scenario = readr::col_character()
        ))
      }
    }
  } else if (is.list(dir)) {
    module_data <- dir[[type]]

    if (!is.null(gaugefilter)) {
      module_data <- module_data |> dplyr::filter(.data$gauge %in% gaugefilter)
    }

    if (!is.null(scenariofilter)) {
      module_data <- module_data |> dplyr::filter(.data$scenario %in% scenariofilter)
    }
  }

  # There are sometimes wholly-blank columns that are read as NA, but should be
  # numeric. We can't pre-set them with readr because they may be in different
  # places for different gauges
  module_data <- module_data |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.logical), as.numeric))

  names(module_data) <- nameclean(names(module_data))

  return(module_data)
}

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

  # One-off particular fixes
  # sometimes the ewr names are ewr_code and sometimes just ewr
  cleannames[cleannames == "ewr" | cleannames == "Code" | cleannames == "code" |cleannames == "ewrCode"] <- "ewr_code"
  # the planning unit names (and IDs) keep getting changed and dropped, so they might be a few different things.
  cleannames[cleannames == "pu" | cleannames == "PlanningUnitName" | cleannames == "planning_unit"] <- "planning_unit_name"
  # The SWSDLName needs to match the sdl_units (and legislation)
  cleannames[cleannames == 's_w_s_d_l_name'] <- 'SWSDLName'

  return(cleannames)
}
