#' Get matches between gauges and planning units Based entirely on the EWR
#' dataframe, NOT actual spatial referencing. That will happen elsewhere. This
#' way ensures internal consistency when dealing with various levels of causal
#' relationships
#' 
#' @param rellist a dataframe or tibble or a list of dataframes/tibbles. At least one of the dataframes needs to have a `"gauge"` column and a `"PlanningUnitID"` column
#'
#' @return a tibble with the PlanningUnitID for each gauge in `rellist`
#' @export
#'
#' @examples
gauge_pu <- function(rellist) {
  
  # handle dataframes/tibbles or a list
  if (is.data.frame(rellist)) {
    g2p <- rellist %>%
      dplyr::select(gauge, PlanningUnitID) %>%
      dplyr::distinct()
    
  } else {
    for (i in rellist) {
      if ('gauge' %in% names(i) & 'PlanningUnitID' %in% names(i)) {
        g2p <- i %>%
          dplyr::select(gauge, PlanningUnitID) %>%
          dplyr::distinct()
        
        break()
      }
    }
    
  }
  
  if (!('g2p' %in% ls())) {
    g2p <- NULL
    warning('no paired gauge and planning unit dataframes')
  }
  
  return(g2p)
  
}
