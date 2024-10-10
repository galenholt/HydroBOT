#' Make a node df from the edges df
#'
#' Takes a dataframe of edges and returns all the from and to nodes and their
#' nodetype, and attach a column indicating their position along the causal
#' sequence
#'
#' @param edgedf a tibble or dataframe of edges with `from` and `to` columns, as
#'   well as `fromtype` and `totype`
#' @param groupers a character vector of grouping variables that might appear in
#'   edgedf. Typically used to retain separate nodes for different planning
#'   units or gauges
#' @param typeorder character vector or dataframe giving the order of the node
#'   types in the network, or `werp` (the default). `werp` flags the use of the
#'   default ranking
#'
#' @return a tibble with columns for `Name` of each individual node, `NodeType`
#'   for the group of nodes of a given type, and `noderoder` specifying where
#'   that nodetype is in the causal flow. May also include grouping column(s)
#'
#' @export
make_nodes <- function(edgedf, groupers = NULL, typeorder = 'werp') {

  # make the default node order- some may not be passed in, but this defines how
  # they should appear on the graph if they exist
  if (length(typeorder) == 1 && typeorder == 'werp') {
    typeorder <- c('ewr_code', 'env_obj', 'Env_obj_main',
                   'Specific_goal', 'Objective', 'Target',
                   'target_5_year_2024', 'target_10_year_2029', 'target_20_year_2039')
  }

  # typeorder could be a df or a character vector
  if (is.character(typeorder)) {
    nodetib <- tibble::tibble(NodeType = typeorder) |>
      dplyr::mutate(nodeorder = dplyr::row_number())
  } else if (is.data.frame(typeorder)) {
    nodetib <- typeorder
  } else {
    stop('typeorder not a df or character vector')
  }



  fromnodes <- edgedf |>
    dplyr::select(tidyselect::any_of(groupers), Name = from, NodeType = fromtype)

  tonodes <- edgedf |>
    dplyr::mutate(nodeorder = edgeorder + 1) |>
    dplyr::select(tidyselect::any_of(groupers), Name = to, NodeType = totype)

  allnodes <- dplyr::bind_rows(fromnodes, tonodes) |>
    dplyr::group_by(Name, NodeType) |>
    dplyr::ungroup() |>
    dplyr::group_by(dplyr::across(tidyselect::any_of(groupers))) |>
    dplyr::distinct() |>
    dplyr::ungroup()

  # order the nodes

  # deal with missing levels- using tibble not vectors because need to join
  # based on nodetype
  realisedorders <- nodetib |>
    dplyr::filter(NodeType %in% unique(allnodes$NodeType)) |>
    dplyr::mutate(shiftorders = (nodeorder - min(nodeorder)) + 1,
           ordersteps = cumsum(c(0, (diff(shiftorders)-1))),
           neworders = shiftorders - ordersteps) |>
    dplyr::select(NodeType, nodeorder = neworders)

  # Join o get the orders set
  allnodes <- dplyr::left_join(allnodes, realisedorders, by = 'NodeType')

  # Remove nodes with NA names- that's not usable
  allnodes <- allnodes |>
    dplyr::filter(!is.na(Name))

  return(allnodes)

}

