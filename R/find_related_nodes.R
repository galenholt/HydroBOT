#' Finds all nodes upstream and downstream of a set of focal nodes
#'
#' Given a small set of nodes, this function finds all related nodes from the
#' beginning to end of the network. It allows branching in both directions, but
#' does not reverse. For example, given a focal node in level 2, it finds all
#' connected nodes in levels 1, 3,4...n, but not other level 2 nodes that are related to
#' the same level 3 nodes.
#'
#' @param edgedf a tibble defining the edges of the network (relationships
#'   between nodes)
#' @param nodenames a character vector of focal nodenames. These do not have to
#'   be within the same level of the network.
#'
#' @return a character vector of nodenames related to the focal nodes
#'
#' @export
#'
#' @examples
#'
find_related_nodes <- function(edgedf, nodenames) {

  # Basically recursive, but with directionality- we don't want all nodes going
  # into or out of the connected nodes- we only want to trace paths that include
  # the selected initial nodes.

  # The tos need to only move in the to direction, ie where do the tos go to,
  # and froms in the from direction

  tonodes <- nodenames
  alltos <- tonodes
  it <- 0
  # we can't step longer than the network
  maxcount <- length(unique(c(edgedf$fromtype, edgedf$totype)))
  while(length(tonodes) > 0 && it < maxcount) {
    tonodes <- edgedf[edgedf$from %in% tonodes, 'to'] |>
      unique() |> dplyr::pull()
    alltos <- c(alltos, tonodes)
    it <- it + 1
  }

  fromnodes <- nodenames
  allfroms <- fromnodes
  it <- 0
  while(length(fromnodes) > 0 && it < maxcount) {
    fromnodes <- edgedf[edgedf$to %in% fromnodes, 'from'] |>
      unique() |> dplyr::pull()
    allfroms <- c(allfroms, fromnodes)
    it <- it + 1
  }



  related_nodes <- c(alltos, allfroms) |> unique()
  return(related_nodes)

}



