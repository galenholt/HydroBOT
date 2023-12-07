# Some simple edges and nodes
edges <- make_edges(dflist = causal_ewr,
                    fromtos = list(c('ewr_code', 'env_obj'),
                                   c('env_obj', 'Specific_goal'),
                                   c('Specific_goal', 'Target'),
                                   c('env_obj', 'target_5_year_2024')),
                    gaugefilter = '409025')
nodes <- make_nodes(edges)


test_that("defaults work", {

  # check the OS
  os_name <- tolower(Sys.info()["sysname"])

  # skipping on ubuntu systems
  if (os_name == "ubuntu") {
    message("Skipping test on Ubuntu systems")
    skip()
  }

  edgecols <- causal_colors_general(edges,
                                   pal_list = list(fromtype = "nationalparkcolors::GeneralGrant"),
                                   colorgroups = NULL, colorset = 'fromtype')

  nodecols <- causal_colors_general(nodes,
                                    pal_list = list(fromtype = "nationalparkcolors::GeneralGrant"),
                                    colorgroups = NULL, colorset = 'NodeType')
  # Looking at it will be the easiest check
  edgeplot <- ggplot2::ggplot(edgecols,
                              ggplot2::aes(x = fromtype,
                                           y = colordef, fill = color)) +
    ggplot2::geom_tile() + ggplot2::scale_fill_identity()

  nodeplot <- ggplot2::ggplot(nodecols,
                              ggplot2::aes(x = NodeType,
                                           y = colordef, fill = fillcolor)) +
    ggplot2::geom_tile() + ggplot2::scale_fill_identity() +
    ggplot2::geom_text(ggplot2::aes(color = fontcolor, label = fontcolor)) +
    ggplot2::scale_color_identity()

  vdiffr::expect_doppelganger("default causal edges", edgeplot)
  vdiffr::expect_doppelganger("default causal nodes", nodeplot)

  # and test groupsizes, since N doesn't come across in the plot
  edgen <- table(edgecols$color)
  noden <- table(nodecols$fillcolor)

  # has to be json-able
  expect_snapshot_value(as.list(edgen))
  expect_snapshot_value(as.list(noden))

})


test_that("scalar colors work and font switching", {


  edgecols <- causal_colors_general(edges,
                                    pal_list = "forestgreen",
                                    colorgroups = NULL, colorset = 'fromtype')

  nodecols <- causal_colors_general(nodes,
                                    pal_list = "cornflowerblue",
                                    colorgroups = NULL, colorset = 'NodeType')
  nodecolsdark <- causal_colors_general(nodes,
                                    pal_list = "navy",
                                    colorgroups = NULL, colorset = 'NodeType')

  # Don't need to plot this
  expect_true(all(edgecols$color == 'forestgreen'))
  expect_true(all(nodecols$fillcolor == 'cornflowerblue'))
  expect_true(all(nodecols$fontcolor == 'black'))
  expect_true(all(nodecolsdark$fillcolor == 'navy'))
  expect_true(all(nodecolsdark$fontcolor == 'white'))


  # and test groupsizes, since N doesn't come across in the plot
  edgen <- table(edgecols$color)
  noden <- table(nodecols$fillcolor)

  # has to be json-able
  expect_snapshot_value(as.list(edgen))
  expect_snapshot_value(as.list(noden))

})

test_that("values work for nodes and edges", {

  # check the OS
  os_name <- tolower(Sys.info()["sysname"])

  # skipping on ubuntu systems
  if (os_name == "ubuntu") {
    message("Skipping test on Ubuntu systems")
    skip()
  }

  edges <- edges |>
    dplyr::mutate(value = dplyr::row_number())
  nodes <- nodes |>
    dplyr::mutate(value = dplyr::row_number())

  edgecols <- causal_colors_general(edges,
                                    pal_list = list(value = 'scico::oslo'),
                                    colorgroups = NULL, colorset = 'value')

  nodecols <- causal_colors_general(nodes,
                                    pal_list = list(value = 'scico::berlin'),
                                    colorgroups = NULL, colorset = 'value')

  # Looking at it will be the easiest check
  edgeplot <- ggplot2::ggplot(edgecols,
                              ggplot2::aes(x = value,
                                           y = colordef, color = color)) +
    ggplot2::geom_point(size = 5) + ggplot2::scale_color_identity()

  # super ugly, but shows the font color too
  nodeplot <- ggplot2::ggplot(nodecols,
                              ggplot2::aes(x = value,
                                           y = colordef, color = fillcolor)) +
    ggplot2::geom_point(size = 5) + ggplot2::scale_color_identity() +
    ggplot2::geom_text(ggplot2::aes(color = fontcolor, label = fontcolor),
                       position = ggplot2::position_nudge(20)) +
    ggplot2::scale_color_identity()

  vdiffr::expect_doppelganger("value edges", edgeplot)
  vdiffr::expect_doppelganger("value nodes", nodeplot)

})

test_that("different palette per group", {

  # check the OS
  os_name <- tolower(Sys.info()["sysname"])

  # skipping on ubuntu systems
  if (os_name == "ubuntu") {
    message("Skipping test on Ubuntu systems")
    skip()
  }

  pal_list_c <- list(ewr_code = 'viridis::mako',
                    env_obj = 'viridis::plasma',
                    Specific_goal = 'scico::oslo',
                    Target = 'scico::hawaii',
                    target_5_year_2024 = 'scico::lisbon')

  edgecols <- causal_colors_general(edges,
                                    pal_list = pal_list_c,
                                    colorgroups = 'fromtype',
                                    colorset = 'from')

  nodecols <- causal_colors_general(nodes,
                                    pal_list = pal_list_c,
                                    colorgroups = 'NodeType',
                                    colorset = 'Name')

  # This is a silly plot that doesn't mean anything, but it works OK to see that
  # each group uses its own palette to label items within it.
  edgeplot <- ggplot2::ggplot(edgecols,
                              ggplot2::aes(x = fromtype,
                                           y = stringr::str_trunc(from, 5),
                                           color = color)) +
    ggplot2::geom_point(size = 5) +
    ggplot2::scale_color_identity()

  nodeplot <- ggplot2::ggplot(nodecols,
                              ggplot2::aes(x = NodeType,
                                           y = stringr::str_trunc(Name, 5),
                                           color = fillcolor)) +
    ggplot2::geom_point(size = 5) + ggplot2::scale_color_identity()

  vdiffr::expect_doppelganger("colors within edge groups", edgeplot)
  vdiffr::expect_doppelganger("colors within node groups", nodeplot)

})
