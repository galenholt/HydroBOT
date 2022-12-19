#' Extracts and arranges the aggregated values for use in causal network plots.
#'
#' This takes the aggregations at each stage along the theme axis,
#' extracts the aggregated values for a given aggregation history (ie the
#' sequence of aggregation functions), and returns in a form that can be added
#' to edges or nodes dfs.
#'
#' @param agglist a list of tibbles of each step in the theme aggregation
#'   (output of `theme_agg_multi` with `saveintermediate = TRUE`). Currently
#'   assumes the aggregation history is already in columns (either
#'   `theme_agg_multi` with `namehistory = FALSE` or have run
#'   `agg_names_to_cols`). Would be fairly easy to detect and add a call to
#'   `agg_names_to_cols` if this becomes an issue.
#' @param whichaggs a character vector of the desired sequence of aggregation
#'   functions. This has to have only values used in the aggregation used to
#'   create `agglist`, and can have only one entry per step (unlike in the
#'   creation of `agglist`). If different aggregation histories are required,
#'   this should be run again, because it is not appropriate to mix histories in
#'   a causal plot.
#' @param valcol character, name of the column with the aggregated values.
#'
#' @return a tibble with grouping variables for scenario and gauge, a column for
#'   values named the same as `valcol`, and `Name` and `NodeType` columns to
#'   allow joining to the node df.
#' @export
#'
#' @examples
extract_vals_causal <- function(agglist, whichaggs, valcol, targetlevels = names(agglist)) {
  # this assumes the history is in columns and not names. Relatively easy to
  # throw the parser on it if not though.
  
  # Will likely need more work to handle list item names once spatial is involved.
  
  # Could almost certainly be a purrr::map()
  stackvalues <- foreach (i = 1:length(agglist),
                          .combine = bind_rows) %do% {
                            
                            # get the name of the nodelevel- the list is named
                            # by which group is aggregated into
                            thesenodes <- targetlevels[i] 
                            
                            # Filter to the right sort of aggregation- noting
                            # that agglist[1] is the raw data and so not
                            # aggregated
                            
                            simpledf <- agglist[[i]]
                            
                            if (i > 1) {
                              # This bit needs to do ALL the n-1 aggs. Not a fan
                              # of the loop, but getting fancy with purr or
                              # pivots was losing history- ie it would get all
                              # ArithmeticMeans in stage 3 no matter what the
                              # previous stages were.
                              
                              for (j in 1:(i-1)) {
                                aggcol <- glue::glue("aggfun_{j}")
                                simpledf <- simpledf %>%
                                  # FIX THIS so it grabs the right aggfuns- noting that step 1 isn't agged. so there's a step-1
                                  filter(across(all_of(aggcol)) == whichaggs[j])
                              }
                              
                            }
                            
                            # Get just the relevant columns- scenario, gauge, the aggregation units, and the values
                            simpledf <- simpledf %>% 
                              dplyr::select(scenario, gauge, all_of(c(thesenodes, valcol))) %>% 
                              dplyr::rename(Name = all_of(thesenodes)) %>% 
                              dplyr::mutate(NodeType = all_of(thesenodes))
                            
                            simpledf
                            
                          }
  
  return(stackvalues)
}

