#' Process objective-long term target links
#'
#' Bespoke function to clean up the current input data
#'
#' @param yrpath path to the file with matched objectives and targets
#' @param saveout logical. saves a file of the output as `outdir/savename_YearMonthDayHourMinute.csv`
#' @param outdir path to the directory to save into. not needed if `saveout` is `FALSE`
#' @param savename one of FALSE, 'r', or 'csv', controls saving and type.
#'   * `FALSE`: (the default): Does not save anything
#'   * `'r'`: saves an rdata file, used primarily for building the data directory in a package
#'   * `'csv'`: saves a csv with the name `outdir/savename_YearMonthDayHourMinute.csv`
#'
#' @return a `tibble` with matching env_objs, Targets, Objectives, and the 5,10, and 20 year targets. No spatial information (gauge, planning unit)
#' @export
#'
clean_long_term <- function(yrpath,
                            saveout = FALSE,
                            outdir, savename) {

  # Need to know where this comes from
  obj2yrtargets <- readr::read_csv(yrpath, col_types = readr::cols())

  # make the names usable
  names(obj2yrtargets) <- names(obj2yrtargets) |>
    stringr::str_remove_all('target') |>
    stringr::str_remove_all(' \\(|\\)') |>
    stringr::str_replace_all('(^[0-9]+)', 'target_\\1') |>
    stringr::str_replace_all(' ', '_') |>
    stringr::str_replace_all('years', 'year') |>
    stringr::str_replace('Env_obj', 'env_obj')

  # clean up weird characters
  suppressWarnings(obj2yrtargets <- obj2yrtargets |>
                     dplyr::select(-c(NodeType, env_obj_main, env_obj_number)) |> # Why was this here?
                     dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringi::stri_enc_toascii(.))) |>
                     dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringr::str_replace_all(.,'\032', '-'))) |>
                     dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringr::str_replace_all(.,'-$', ''))) |>
                     dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~stringr::str_squish(.))))

  # now that we have multiple states, but a state on there.
  # save
  if (saveout == 'r') {

    # Rdata for package structure
    saveRDS(obj2yrtargets, file = file.path(outdir, 'obj2yrtargets.rds'))
  } else if (saveout == 'csv') {

    # csv for other
    readr::write_csv(obj2yrtargets,
              file.path(outdir,
                        paste0(savename,
                               format(Sys.time(),
                                      "%Y%m%d%H%M"),
                               ".csv")))
  }


  return(obj2yrtargets)
}
