#' Extract the ewrs for each gauge, along with the PlanningUnitID and
#' LTWPShortName
#'
#' Pulls straight from py-ewr, so very useful for various cross-referencing and
#' keeping up to date with the state of that package.
#'
#' @return dataframe with `PlanningUnitID`, `LTWPShortName`, `gauge`, and
#'   `ewr_code` columns as they exist in the good EWR table from py-ewr
#' @export
#'
#' @examples
#' gauge_ltwp_ewr()
gauge_ltwp_ewr <- function() {
  pdi <- reticulate::import("py_ewr.data_inputs")
  ewrs_in_pyewr <- pdi$get_EWR_table()[[1]] |> # Just the good ones
    dplyr::select(PlanningUnitID, LTWPShortName, gauge = Gauge, ewr_code = Code)
}

#' Extract the gauge to planning unit match from a dataframe.
#'
#' This is not spatial, it gets the relationship from a passed in df. This way
#' ensures internal consistency when dealing with various levels of causal
#' relationships
#'
#' @param rellist a dataframe or tibble or a list of dataframes/tibbles. At
#'   least one of the dataframes needs to have a `"gauge"` column and a
#'   `"PlanningUnitID"` column
#'
#' @return a tibble with the PlanningUnitID for each gauge in `rellist`
#' @export
#'
#' @examples
extract_gauge_pu <- function(rellist) {

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


