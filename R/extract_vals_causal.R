#' Extracts and arranges the aggregated values for use in causal network plots.
#'
#' This takes the aggregations at each stage along the theme axis, extracts the
#' aggregated values for a given aggregation history (ie the sequence of
#' aggregation functions), and returns in a form that can be added to edges or
#' nodes dfs.
#'
#' @param agglist a list of tibbles of each step in the theme aggregation
#'   (output of `theme_agg_multi` with `saveintermediate = TRUE`). Currently
#'   assumes the aggregation history is already in columns (either
#'   `theme_agg_multi` with `namehistory = FALSE` or have run
#'   `agg_names_to_cols`). Would be fairly easy to detect and add a call to
#'   `agg_names_to_cols` if this becomes an issue.
#' @param whichaggs a character vector or list with one string per list item of
#'   the desired sequence of aggregation functions. This has to have only values
#'   used in the aggregation used to create `agglist`, and can have only one
#'   entry per step (unlike in the creation of `agglist`). If different
#'   aggregation histories are required, this should be run again, because it is
#'   not appropriate to mix histories in a causal plot.
#' @param valcol character, name of the column with the aggregated values.
#' @param targetlevels character, names of levels to include. Defaults to
#'   `names(agglist)`. Any subset of those names can be used.
#'
#' @return a tibble with grouping variables for scenario and gauge, a column for
#'   values named the same as `valcol`, and `Name` and `NodeType` columns to
#'   allow joining to the node df.
#' @export
#'
extract_vals_causal <- function(agglist, whichaggs, valcol, targetlevels = names(agglist)) {
  # this assumes the history is in columns and not names. Relatively easy to
  # throw the parser on it if not though.

  # Will likely need more work to handle list item names once spatial is involved.

  # Filter to just the targets
  agglist <- agglist[names(agglist) %in% targetlevels]

  # Could almost certainly be a purrr::map()
  stackvalues <- foreach::foreach (i = 1:length(targetlevels),
                          .combine = dplyr::bind_rows) %do% {

                            # get the name of the nodelevel- the list is named
                            # by which group is aggregated into
                            thesenodes <- targetlevels[i]
                            # The -1 is because agglist[1] is the raw data and so not
                            # aggregated
                            aggindex <- which(names(agglist) == thesenodes)-1

                            if (length(thesenodes) > 1) {
                              rlang::abort(glue::glue("Expect each node level to have one value,
                                                      this one {agglist[i]} is {thesenodes}"))
                            }

                            # Filter to the right sort of aggregation
                            simpledf <- agglist[[thesenodes]]

                            if (aggindex > 1) {
                              # This bit needs to do ALL the n-1 aggs. Not a fan
                              # of the loop, but getting fancy with purr or
                              # pivots was losing history- ie it would get all
                              # ArithmeticMeans in stage 3 no matter what the
                              # previous stages were.

                              for (j in 1:(aggindex)) {
                                aggcol <- paste0("aggfun_", as.character(j))
                                colind <- which(names(simpledf) == aggcol)
                                simpledf <- simpledf |>
                                  dplyr::filter(.data[[aggcol]] == whichaggs[[j]])
                              }

                            }

                            # Get just the relevant columns- scenario, gauge, the aggregation units, and the values
                            # Don't assume scenario and gauge exist though
                            simpledf <- simpledf |>
                              dplyr::select(tidyselect::any_of(c('scenario', 'gauge')),
                                            tidyselect::all_of(c(thesenodes, valcol))) |>
                              dplyr::rename(Name = tidyselect::all_of(thesenodes)) |>
                              dplyr::mutate(NodeType = thesenodes)

                            simpledf

                          }

  return(stackvalues)
}

