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
#' @examples
clean_long_term <- function(yrpath,
                            saveout = FALSE,
                            outdir, savename) {
  
  # Need to know where this comes from
  obj2yrtargets <- readr::read_csv(yrpath, col_types = cols())
  
  # make the names usable
  names(obj2yrtargets) <- names(obj2yrtargets) %>% 
    str_remove_all('target') %>% 
    str_remove_all(' \\(|\\)') %>%
    str_replace_all('(^[0-9]+)', 'target_\\1') %>% 
    str_replace_all(' ', '_') %>% 
    str_replace_all('years', 'year') %>% 
    str_replace('Env_obj', 'env_obj')
    
  # clean up weird characters
  suppressWarnings(obj2yrtargets <- obj2yrtargets %>% 
                     select(-c(NodeType, env_obj_main, env_obj_number)) %>% # Why was this here?
                     mutate(across(where(is.character), ~stringi::stri_enc_toascii(.))) %>%
                     mutate(across(where(is.character), ~str_replace_all(.,'\032', '-'))) %>% 
                     mutate(across(where(is.character), ~str_replace_all(.,'-$', ''))) %>% 
                     mutate(across(where(is.character), ~str_squish(.))))
  
  # save
  if (saveout == 'r') {
    
    # Rdata for package structure
    save(obj2yrtargets, file = file.path(outdir, 'obj2yrtargets.rdata'))
  } else if (saveout == 'csv') {
    
    # csv for other
    write_csv(obj2yrtargets, 
              file.path(outdir,
                        paste0(savename,
                               format(Sys.time(),
                                      "%Y%m%d%H%M"),
                               ".csv")))
  }
  
  
  return(obj2yrtargets)
}