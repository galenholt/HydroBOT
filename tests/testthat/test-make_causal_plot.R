# Setup -------------------------------------------------------------------
skip_on_os('linux')

agg_theme_space <- make_test_agg(namehistory = FALSE)

# the sequences used in make_test_agg
aggseq <- list(ewr_code = c('ewr_code_timing', 'ewr_code'),
               env_obj =  c('ewr_code', "env_obj"),
               sdl_units = sdl_units,
               Specific_goal = c('env_obj', "Specific_goal"),
               catchment = cewo_valleys,
               Objective = c('Specific_goal', 'Objective'),
               mdb = basin,
               target_5_year_2024 = c('Objective', 'target_5_year_2024'))

funseq <- list('CompensatingFactor',
               'ArithmeticMean',
               'ArithmeticMean',
               "ArithmeticMean",
               list(wm = ~weighted.mean(., w = area,
                                        na.rm = TRUE)),
               'ArithmeticMean',
               list(wm = ~weighted.mean(., w = area,
                                        na.rm = TRUE)),
               'ArithmeticMean')

# extract theme steps
themesteps <- purrr::map_lgl(aggseq, is.character)


themeseq <- aggseq[themesteps]

themefuns <- funseq[themesteps]

ewr_edges <- make_edges(dflist = causal_ewr,
                        fromtos = themeseq[2:length(themeseq)],
                        gaugefilter = "421001")

nodes <- make_nodes(ewr_edges)

# What is the column that defines the value?
valcol <- 'ewr_achieved'

# Get the values for each node
targetlevels <- names(themesteps)[themesteps]

aggvals <- extract_vals_causal(agg_theme_space, themefuns, valcol,
                               targetlevels = targetlevels)

# cut to relevant gauge (or no gauge for higher spatial levels)
# THis is slow- I think what we'd actually do is find the matching sdl and cut to that earlier
gaugematch <-  st_intersects(bom_basin_gauges[bom_basin_gauges$gauge == '421001',], aggvals, sparse = FALSE)

aggvals <- aggvals[as.vector(gaugematch),] |>
  st_drop_geometry()
# aggvals <- aggvals %>% dplyr::filter(gauge == '421001' | is.na(gauge)) %>%
#   dplyr::select(-gauge)

# join to the nodes
nodes_with_vals <- nodes |>
  dplyr::filter(NodeType != 'ewr_code_timing') |>
  dplyr::left_join(aggvals) |>
  dplyr::filter(!is.na(scenario))

svgnetwriter <- function(plot, file, title = "") {
  plot |>
    DiagrammeR::export_graph(file_name = paste0(file))
}

# trying to check vdiffr
# title <- 'aggNetworkdown'
# fig <- aggNetworkdown
# fig_name <- vdiffr:::str_standardise('aggNetworkdown')
# file <- paste0(fig_name, ".svg")
# testcase <- vdiffr:::make_testcase_file(fig_name)
# svgnetwriter(fig, testcase, title)

# tests -------------------------------------------------------------------


test_that("networks work", {
  aggNetworkdown <- make_causal_plot(nodes = dplyr::filter(nodes_with_vals,
                                                           scenario == 'down4_down4'),
                                     edges = dplyr::filter(ewr_edges, gauge %in% unique(nodes_with_vals$gauge)),
                                     edge_pal = 'black',
                                     node_pal = list(value = 'scico::tokyo'),
                                     node_colorset = 'ewr_achieved',
                                     render = FALSE)
  # DiagrammeR::render_graph(aggNetworkdown)

  vdiffr::expect_doppelganger("aggNetworkdown", aggNetworkdown, writer = svgnetwriter)
})

