test_that("nodeorder", {
  testset <- causal_ewr$ewr2obj |>
    dplyr::filter(grepl('Macq', LTWPShortName)) |>
    # dplyr::select(-ewr_code_timing, -state, -LTWPShortName, -env_obj) |>
    dplyr::distinct()

  testedges <- make_edges(dflist = list(testset),
                          fromtos = list(Target = c('ewr_code', 'Target')),
                          gaugefilter = '421019')

  testnodes <- make_nodes(testedges)
})
