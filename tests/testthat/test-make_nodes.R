test_that("nodeorder", {
  testset <- causal_ewr$theme_to_ewr_code_main |>
    dplyr::filter(grepl('Macq', LTWPShortName)) |>
    # dplyr::select(-ewr_code_timing, -state, -LTWPShortName, -env_obj) |>
    dplyr::distinct()

  testedges <- make_edges(dflist = list(testset),
                          fromtos = list(theme = c('ewr_code_main', 'theme')),
                          gaugefilter = '421019')

  testnodes <- make_nodes(testedges)

  expect_equal(names(testnodes), c('Name', 'NodeType', 'nodeorder'))
  expect_s3_class(testnodes, 'tbl_df')
})
