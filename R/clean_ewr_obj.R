#' Pair EWR indicators to environmental objectives
#'
#'  DEPRECATED- use [get_causal_ewr()]. Bespoke function to clean the EWR table and LTWP table, extract environmental objectives, and pair them. Will need to change or at least extensively retest if those datasets change
#'
#' @param ewrobjpath path to the ewr-obj mapping. Default 'ewrtool' just uses the version in the tool.
#' @param gaugescale logical, whether to return data at the gauge scale by remapping from LTWPShortName to gauge (`TRUE`), or to just leave at the LTWPShortName (e.g. sdl unit) scale as NSW now defines the relationships (`FALSE`)
#' @param saveout one of FALSE, 'r', or 'csv', controls saving and type.
#'   * `FALSE`: (the default): Does not save anything
#'   * `'r'`: saves an rdata file, used primarily for building the data directory in a package
#'   * `'csv'`: saves a csv with the name `outdir/savename_YearMonthDayHourMinute.csv`
#' @param outdir path to the directory to save into. not needed if `saveout` is `FALSE`
#' @param savename character for the filename to save. the date and time gets appended to avoid overwriting
#'
#' @return A `tibble` with columns for `LTWPShortName`, `ewr_code`, `ewr_code_timing`, and `env_obj` (if `gaugescale = FALSE`), and including `PlanningUnitID` and `gauge` if `gaugescale = TRUE`)
#' @keywords internal
clean_ewr_obj <- function(ewrobjpath = 'ewrtool',
                          gaugescale = TRUE,
                          saveout = FALSE,
                          outdir, savename) {

  rlang::warn("ewr causals are now provided by py_ewr, obtain with `get_causal_ewr()`. This is provided for historical purposes but will likely be deprecated soon.")

  if (ewrobjpath == 'ewrtool') {
    # the python in the EWR tool that gives `get_ewr_table` drops the columns we need, so get the sheet directly
    objective_mapping <- get_raw_ewrsheet() |>
      cleanewrs() |>
      dplyr::select("planning_unit_name", "gauge", "ewr_code", "ewr_code_timing",
                    "eco_objective_code", "high_level", "state",
                    LTWPShortName = "l_t_w_p_short_name")

    # we can't do a good job linking these yet, so make a few versions that will do the best we can.
    ewr2obj_embedded <- objective_mapping |>
      dplyr::select(-"high_level") |>
      dplyr::mutate(env_obj = strsplit(.data$eco_objective_code, split = '_')) |>
      tidyr::unnest_longer(col = "env_obj") |>
      dplyr::select(-"eco_objective_code")

    ewr2obj <- ewr2obj_embedded |>
      dplyr::mutate(Target = dplyr::case_when(grepl('^NF', .data$env_obj) ~ "Native fish",
                                       grepl('^NV', .data$env_obj) ~ "Native vegetation",
                                       grepl('^OS', .data$env_obj) ~ "Other species",
                                       grepl('^EF', .data$env_obj) ~ "Priority ecosystem function",
                                       grepl('^WB', .data$env_obj) ~ "Waterbird",
                                       .default = NA))
  } else {
    # read in and minor cleanup
    ewr2obj <- readr::read_csv(ewrobjpath, show_col_types = FALSE)  |>
      dplyr::rename(ewr_code = "EWR",
                    # These are nearly sdl units, but the name 'LTWPShortName' comes from the EWR tool itself
                    LTWPShortName = "Planning_area") |>
      dplyr::distinct()

    # separate out the Objectives into rows- we want a row for each Planning_area, EWR, Objective combo.
    ewr2obj <- ewr2obj |>
      # Some annoying typos with spaces and ., and we need to split the comma seps
      dplyr::mutate(env_obj = stringr::str_remove_all(.data$Objectives, ' ')) |>
      dplyr::mutate(env_obj = stringr::str_split(.data$env_obj, ',|\\.')) |>
      dplyr::select(-"Objectives") |>
      tidyr::unnest_longer(col = "env_obj")

    ewr2obj <- ewr2obj |>
      separate_ewr_codes()

    if (gaugescale) {
      # Expand out to gauge scale- give the relevant LTWP area and ewr_code to each gauge and PlanningUnit
      # This may not be needed here, but it retains maximal information, including some that got lost in the switch to NSW ewr-obj mapping
      ewrs_in_pyewr <- get_ewr_table() |>
        dplyr::select("PlanningUnitID", planning_unit_name = "PlanningUnitName",
                      "LTWPShortName", gauge = "Gauge", ewr_code = "Code") |>
        separate_ewr_codes()

      ewr2obj <- dplyr::left_join(ewrs_in_pyewr, ewr2obj, by = c('LTWPShortName', 'ewr_code', 'ewr_code_timing'))

    }

    # don't save NAs and make it a tibble
    ewr2obj <- ewr2obj |>
      dplyr::filter(!is.na(.data$ewr_code) & !is.na(.data$env_obj)) |>
      tibble::tibble()

    attr(ewr2obj, "pandas.index") <- NULL
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
