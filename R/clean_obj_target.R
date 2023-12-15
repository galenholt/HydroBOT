#' Pair environmental objectives to various targets and objectives
#'
#' Bespoke function to clean up the current input data
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
#' @export
#'
#' @examples
clean_obj_target <- function(ewrobjs,
                              targetpath,
                              qcfiles,
                             saveout = FALSE,
                             outdir, savename) {

  # This cuts to only a single row per planning unit/objective pair and drops the ewrs
  objpu <- ewrobjs |>
    dplyr::select(tidyselect::any_of(c('PlanningUnitID', 'planning_unit_name', 'LTWPShortName')), env_obj) |>
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
    dplyr::mutate(Objective = stringr::str_remove_all(Objective, '\'')) |>
    dplyr::rename(env_obj = Env_obj) |>
    dplyr::select(-NodeType)


  # Expands the objective to target mapping to all relevant planning units
  obj2target <- dplyr::full_join(objpu, targets,  by = "env_obj", relationship = "many-to-many")

  # adjust for manual QC from Renee This is separate because it's not ideal way
  # to do the QC adjustment.

  obj2target <- obj2target |>
    dplyr::filter(!LTWPShortName == "Murray Lower Darling")|> #REmove the Murray to add in the data that Renee has already checked
    dplyr::filter(!(LTWPShortName == "Macquarie-Castlereagh" & Macquarie_Castlereagh == 0))|>
    dplyr::filter(!(LTWPShortName == "Murrumbidgee" & Murrumbidgee == 0)) |>
    dplyr::select(-c(Murray_Lower_Darling,	Macquarie_Castlereagh,	Murrumbidgee))

  #add in the data that Renee has already checked these have already been
  #checked.

  # THe provenance of these files is unknown. It doesn't make sense we
  # need to do this weird multi-level joining just to get the names?
  # warnings suppressed because there's an annoying first column that gets a new name
  qc_fix <- readr::read_csv(qcfiles[1], col_types = readr::cols(), col_select = -1) |>
    dplyr::rename(Specific_goal = Target.species,
           env_obj = Env_obj) |>
    dplyr::select(-NodeType)

  PUs_names <- readr::read_csv(qcfiles[2], col_types = readr::cols(), col_select = -1)

  qc_fix <- dplyr::left_join(qc_fix, PUs_names, by = "PU", relationship = 'many-to-many')
  #need to check with Renee - these are not objective specific but macquarie seem to be.

  # This is crazy how much re-joining we're doing. Need to find where all this
  # came from and just build it cleanly
  pu2ltwp <- objpu |>
    dplyr::select(-env_obj) |>
    dplyr::distinct()

  qc_fix <- dplyr::left_join(qc_fix, pu2ltwp, by = 'LTWPShortName', relationship = 'many-to-many')

  qc_fix <- qc_fix |>
    dplyr::filter(link != 0 & !is.na(LTWPShortName)) |>  #watch out for the 2s = Renee changes
    dplyr::select(-c(link, PU, PlanningUnitName)) |>  # get rid of extra cols not in the main data
    dplyr::distinct()

  obj2target <- dplyr::bind_rows(obj2target, qc_fix)

  # cleanup column ordering
  obj2target <- obj2target |>
    dplyr::select(any_of(c('PlanningUnitID', 'planning_unit_name')),
                  LTWPShortName, env_obj, Specific_goal, Objective, Target)

  # final cleanup of weird characters and dplyr::rename to standard
  suppressWarnings(obj2target <- obj2target |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringi::stri_enc_toascii(.))) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringr::str_replace_all(.,'\032', '-'))) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringr::str_replace_all(.,'-$', ''))) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringr::str_squish(.))))

  obj2target <- obj2target |>
    dplyr::filter(!is.na(env_obj)) |>
    dplyr::distinct()


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
