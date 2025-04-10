#' Pair environmental objectives to various targets and objectives
#'
#' DEPRECATED- use [get_causal_ewr()].Bespoke function to clean up the current input data.
#'
#' @param ewrobjs `tibble` or `data.frame` of the EWR indicators paired to the environmental objectives. Should be the "long" output from `clean_ewr_obj`
#' @param targetpath path to the table linking objectives to targets
#' @param qcfiles list of paths to QC files from Renee
#' @param saveout one of FALSE, 'r', or 'csv', controls saving and type.
#'   * `FALSE`: (the default): Does not save anything
#'   * `'r'`: saves an rdata file, used primarily for building the data directory in a package
#'   * `'csv'`: saves a csv with the name `outdir/savename_YearMonthDayHourMinute.csv`
#' @param outdir path to the directory to save into. not needed if `saveout` is `FALSE`
#' @param savename character for the filename to save. the date and time gets appended to avoid overwriting
#'
#' @return a `tibble` linking the `env_obj` to several different outcomes- `Specific_goal` (roughly species, but also things like refugia), `Objective` (bigger-picture, e.g. no loss of native fish species), and `Target` (Native Fish, Ecosystem Function, etc). At the Planning Unit scale.
#' @keywords internal
clean_obj_target <- function(ewrobjs,
                              targetpath,
                              qcfiles,
                             saveout = FALSE,
                             outdir, savename) {
  rlang::warn("ewr causals are now provided by py_ewr, obtain with `get_causal_ewr()`. This is provided for historical purposes but will likely be deprecated soon.")


  # This cuts to only a single row per planning unit/objective pair and drops the ewrs
  objpu <- ewrobjs |>
    dplyr::select(tidyselect::any_of(c('PlanningUnitID', 'planning_unit_name', 'LTWPShortName', 'state')),
                  .data$env_obj) |>
    dplyr::distinct()

  # This csv has the relationships from objectives to a few other things- target groups, species and other things like refugia, 'Objectives'
  targets <- readr::read_csv(targetpath, col_types = readr::cols())

  # minor cleanup
  names(targets)[which(names(targets) == "Target species")] <- 'Specific_goal'
  names(targets) <- names(targets) |>
    stringr::str_replace_all(' ', '_') |>
    stringr::str_replace_all('-', '_')


  # annoying that there are special characters, but since I don't know where the
  # data comes from I can't clean it on that side
  targets <- targets |>
    dplyr::mutate(Objective = stringr::str_remove_all(.data$Objective, '\'')) |>
    dplyr::rename(env_obj = .data$Env_obj) |>
    dplyr::select(-.data$NodeType) |>
    dplyr::mutate(state = 'NSW')


  # Expands the objective to target mapping to all relevant planning units
  obj2target <- dplyr::full_join(objpu, targets,  by = c("env_obj", 'state'), relationship = "many-to-many")

  # adjust for manual QC from Renee This is separate because it's not ideal way
  # to do the QC adjustment.

  obj2target <- obj2target |>
    dplyr::filter(!.data$LTWPShortName == "Murray Lower Darling")|> #REmove the Murray to add in the data that Renee has already checked
    dplyr::filter(!(.data$LTWPShortName == "Macquarie-Castlereagh" & .data$Macquarie_Castlereagh == 0))|>
    dplyr::filter(!(.data$LTWPShortName == "Murrumbidgee" & .data$Murrumbidgee == 0)) |>
    dplyr::select(-c(.data$Murray_Lower_Darling,	.data$Macquarie_Castlereagh,	.data$Murrumbidgee))

  #add in the data that Renee has already checked these have already been
  #checked.

  # THe provenance of these files is unknown. It doesn't make sense we
  # need to do this weird multi-level joining just to get the names?
  qc_fix <- readr::read_csv(qcfiles[1], col_types = readr::cols(), col_select = -1) |>
    dplyr::rename(Specific_goal = .data$Target.species,
           env_obj = .data$Env_obj) |>
    dplyr::select(-.data$NodeType)

  PUs_names <- readr::read_csv(qcfiles[2], col_types = readr::cols(), col_select = -1)

  qc_fix <- dplyr::left_join(qc_fix, PUs_names, by = "PU", relationship = 'many-to-many')
  #need to check with Renee - these are not objective specific but macquarie seem to be.

  # This is a lot of re-joining.Would be better to just build it cleanly originally, but the QC was by hand
  pu2ltwp <- objpu |>
    dplyr::select(-.data$env_obj) |>
    dplyr::distinct()

  qc_fix <- dplyr::left_join(qc_fix, pu2ltwp, by = 'LTWPShortName', relationship = 'many-to-many')

  qc_fix <- qc_fix |>
    dplyr::filter(.data$link != 0 & !is.na(.data$LTWPShortName)) |>  #watch out for the 2s = Renee changes
    dplyr::select(-c(.data$link, .data$PU, .data$PlanningUnitName)) |>  # get rid of extra cols not in the main data
    dplyr::distinct()

  obj2target <- dplyr::bind_rows(obj2target, qc_fix)

  # cleanup column ordering
  obj2target <- obj2target |>
    dplyr::select(tidyselect::any_of(c('PlanningUnitID', 'planning_unit_name')),
                  .data$LTWPShortName, .data$env_obj, .data$Specific_goal,
                  .data$Objective, .data$Target, .data$state)

  # final cleanup of weird characters and dplyr::rename to standard
  obj2target <- obj2target |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringi::stri_enc_toutf8(.))) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringr::str_replace_all(.,'\xca', '-'))) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringr::str_replace_all(.,'-$', ''))) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringr::str_squish(.)))

  obj2target <- obj2target |>
    dplyr::filter(!is.na(.data$env_obj)) |>
    dplyr::distinct()

  # infer the ones that aren't there.
  obj2target <- obj2target |>
    dplyr::mutate(Target = dplyr::case_when(
      is.na(.data$Target) & grepl('^NF', .data$env_obj) ~ "Native fish",
      is.na(.data$Target) & grepl('^NV', .data$env_obj) ~ "Native vegetation",
      is.na(.data$Target) & grepl('^OS', .data$env_obj) ~ "Other species",
      is.na(.data$Target) & grepl('^EF', .data$env_obj) ~ "Priority ecosystem function",
      is.na(.data$Target) & grepl('^WB', .data$env_obj) ~ "Waterbird",
      .default = .data$Target
    ))


  # save
  if (saveout == 'r') {

    # Rdata for package structure. enforce naming here
    saveRDS(obj2target, file = file.path(outdir, 'obj2target.rds'))

  } else if (saveout == 'csv') {

    # csv for other
    readr::write_csv(obj2target,
              file.path(outdir,
                        paste0(savename,
                               format(Sys.time(),
                                      "%Y%m%d%H%M"),
                               ".csv")))
  }

  return(obj2target)

}
