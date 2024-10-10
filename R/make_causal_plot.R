#' Builds a causal network plot
#'
#' Takes node and edges dfs, along with arguments to specify attributes
#' (particularly color), and how to return the output. Internally, this finds
#' the network relevant to a given set of nodes (by default all of them), using
#' `find_related_nodes`, attributes according to the attribute arguments using
#' `causal_colors_general` and `node_plot_atts` and then uses `DiagrammeR` to
#' make the plot.
#'
#' @param nodes a tibble or dataframe defining nodes
#' @param edges a tibble or dataframe defining all edge relationships
#' @param focalnodes a character vector of nodes to use to find the related
#'   network (default is all values in the `Name` column of the `nodes` df).
#'   Uses `find_related_nodes` to build a network with these nodes along with
#'   all their upstream and downstream nodes.
#' @param drop_unused_nodes logical, default `TRUE`, drops nodes with no
#'   connections. `FALSE` could be useful to keep unconnected nodes if we want
#'   to see if there are any that are not causally related to the others.
#' @param edge_pal a named list of color palettes or color values for the
#'   edges.  Currently only supports paletteer palettes, vectors of colors, or
#'   single colors. See \code{\link{causal_colors_general}}
#' @param edge_colorgroups NULL (the default) or a column name (character)
#'   specifying a grouping column for different color palettes for the edges.
#'   See \code{\link{causal_colors_general}}
#' @param edge_colorset a column name (character) giving the column to define
#'   edge color. See \code{\link{causal_colors_general}}
#' @param node_pal a named list of color palettes or color values for the
#'   nodes. Currently only supports paletteer palettes, vectors of colors, or
#'   single colors. See \code{\link{causal_colors_general}}
#' @param node_colorgroups NULL (the default) or a column name (character)
#'   specifying a grouping column for different color palettes for the nodes.
#'   See \code{\link{causal_colors_general}}
#' @param node_colorset a column name (character) giving the column to define
#'   node color. See \code{\link{causal_colors_general}}
#' @param setLimits NULL (default) or length-2 numeric vector to force limits of the color scale.
#' @param wrap_names logical, default `TRUE`, should long node names be wrapped
#'   to multiple lines
#' @param render logical, default `TRUE`, should the network be rendered to the
#'   graphics device?
#' @param returnnetwork logical, default `TRUE`, should the graph object
#'   produced by `DiagrammeR` be returned?
#' @param save logical, default `FALSE`, should the graph be saved as a .png and
#'   .pdf
#' @param savedir path to directory for saving outputs. Only needed if `save =
#'   TRUE`
#' @param savename character, filename for the output plots.
#'    .pdf and .png are automatically appended.
#' @param edge_pal_direction either 1 (default) or -1 (reversed) direction of the palettes
#' @param node_pal_direction either 1 (default) or -1 (reversed) direction of the palettes
#'
#' @return causal network in DiagrammeR format
#' @export
#'
make_causal_plot <- function(nodes, edges,
                             focalnodes = unique(nodes$Name),
                             drop_unused_nodes = TRUE,
                             edge_pal = list(fromtype = "nationalparkcolors::GeneralGrant"),
                             edge_colorgroups = NULL,
                             edge_colorset = 'fromtype',
                             edge_pal_direction = rep(1, length(edge_pal)),
                             node_pal = list(fromtype = "nationalparkcolors::GeneralGrant"),
                             node_colorgroups = NULL,
                             node_colorset = 'NodeType',
                             node_pal_direction = rep(1, length(node_pal)),
                             setLimits = NULL,
                             wrap_names = TRUE,
                             render = TRUE,
                             returnnetwork = TRUE,
                             save = FALSE, savedir, savename = NULL) {
  # Function to make plots assuming standard WERP column names. Could allow name
  # passing I guess? But then what's the point of a function if it ends up just
  # looking like the call

  # Make sure no duplicate nodes
  nodes <- nodes |>
    dplyr::distinct(Name, .keep_all = TRUE)

  # IF WE"RE GOING TO WRAP NAMES IT NEEDS TO HAPPEN BY HERE or the rest of this doesn't match
  if (wrap_names) {
    edges <- edges |>
      dplyr::mutate(from = stringr::str_wrap(from, 40),
             to = stringr::str_wrap(to, 40))
    nodes <- nodes |>
      dplyr::mutate(Name = stringr::str_wrap(Name, 40))
  }

  # Cut both to the desired nodeset
  relevantnodes <- find_related_nodes(edges, focalnodes)

  nodes <- nodes |>
    dplyr::filter(Name %in% relevantnodes)

  edges <- edges |>
    dplyr::filter(from %in% relevantnodes | to %in% relevantnodes)

  # drop nodes with no edges?
  if (drop_unused_nodes) {
    nodes <- nodes |>
      dplyr::filter(Name %in% edges$from | Name %in% edges$to)
  }

  # colors
  edges <- causal_colors_general(df = edges,
                                 pal_list = edge_pal,
                                 pal_direction = edge_pal_direction,
                                   colorgroups = edge_colorgroups,
                                 colorset = edge_colorset,
                                 setLimits = setLimits)

  nodes <- causal_colors_general(df = nodes,
                                 pal_list = node_pal,
                                 pal_direction = node_pal_direction,
                                 colorgroups = node_colorgroups,
                                 colorset = node_colorset,
                                 setLimits = setLimits)

  # deal with names, position, shape etc
  nodes <- node_plot_atts(nodes)


  # Syntax for diagrammer is confusing.
  # table and node are env variables (dfs)
  # label_col, type_col, from_col, and to_col are data variables (column names)
  # from_to_map is just an argument that says to label the fromto, it's not an env or data variable
  # The attributes (color etc) are not called out here at all, and depend on having the right column names in the dfs

  # Cryptic errors here related to `c.c` and extra rows seem to be caused by having rows that are duplicated over the Name and NodeType columns (even if there are differences elsewhere)
  causalnetwork <-
    DiagrammeR::create_graph() |>
    DiagrammeR::add_nodes_from_table(
      table = nodes,
      label_col = Name,
      type_col = NodeType) |>
    DiagrammeR::add_edges_from_table(
      table = edges,
      from_col = from,
      to_col = to,
      from_to_map = label) |>

    DiagrammeR::add_global_graph_attrs(
      attr = c("layout", "splines"),
      value = c("neato", "true"),
      attr_type = c("graph", "graph"))

  if (render) {
    print(causalnetwork |>
            DiagrammeR::render_graph())
  }

  if (save) {
    causalnetwork |>
      DiagrammeR::export_graph(file_name = file.path(savedir, stringr::str_c(savename,
                                                        '.png')))


    causalnetwork |>
      DiagrammeR::export_graph(file_name = file.path(savedir, stringr::str_c(savename,
                                                        '.pdf')))
  }

  if (returnnetwork) {
    return(causalnetwork)
  }

}
