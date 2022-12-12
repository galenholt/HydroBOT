#' Change aggregation history in col names to stepwise columns
#'
#' By default, aggregation history (function and level) is tracked in the column
#' names for memory purposes and to keep units clear. But it's usually more
#' useful to be able to access that information, so this parses the names of the
#' aggregated columns into columns for the aggregation function at each
#' aggregation level.
#'
#' @param aggdf aggregated dataframe or tibble
#' @param aggsequence a list of the names of the aggregation. works best if the list passed to the calling function is named, and those names are passed in here.Typically it will be the second value in each theme aggregation step (the to-theme), or the name of the spatial polygons. But extracting those when unnamed isn't well tested.
#' @param funsequence a list of functions (as characters or lists of names and arguments) to apply to do the aggregation at each step defined in `aggsequence`. Can be multiple funs per aggregation step.
#' @param aggCols the name(s) of the original columns that were aggregated.
#'
#' @return a tibble with a column named as the original input data at the start of aggregation, and columns for every aggregation step giving the level aggregated into and the function used for the aggregation. if multiple functions are used, the rows are stacked (long format).
#' @export
#'
#' @examples
#' 
agg_names_to_cols <- function(aggdf, aggsequence, funsequence, aggCols) {
  
  # suppressWarnings probably a bad idea, but there's a lot of really pointless warnings that get thrown
  suppressWarnings(
  aggincols <- aggdf %>% 
    pivot_longer(ends_with(aggCols)) %>% 
    # This removes the aggregation level name ('ewr_code', 'catchment', etc)
    mutate(allfuns = str_remove_all(name, 
                                    str_flatten(str_c('(', unlist(aggsequence), ')'), 
                                                collapse = '|')),
           allfuns = str_remove(allfuns, '^_')) %>%
    separate(allfuns, into = paste0('aggfun_', length(funsequence):1), sep = '__') %>% 
    separate(aggfun_1, into = c('aggfun_1', 'original'), sep = '_', extra = 'merge') %>% 
    mutate(alllevs = str_remove_all(name, 
                                    str_flatten(str_c('(', unlist(funsequence), ')'), 
                                                collapse = '|'))) %>% 
    separate(alllevs, into = paste0('aggLevel_', length(funsequence):1), sep = '__') %>% 
    select(-name) %>% 
    # sf seems to be behind tidyverse, and needs characters here instead of bare
    # names. Both seem to work for normal df/tibbles
    pivot_wider(names_from = 'original', values_from = 'value') %>% 
    select(!ends_with(str_c('_', as.character(1:length(funsequence)))), 
           ends_with(str_c('_', as.character(1:length(funsequence)))))
  )
  
  return(aggincols)
}

