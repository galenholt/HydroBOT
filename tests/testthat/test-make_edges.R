test_that("making edges works", {
  aggseq <- list(c('ewr_code', 'ewr_code_main'),
                 c('ewr_code_main', "eco_objective"),
                 c('eco_objective', "objective_text"),
                 c('objective_text', 'theme'))

  edges <- make_edges(causal_ewr, aggseq)
  expect_equal(names(edges), c('gauge', 'planning_unit_name', 'SWSDLName', 'from', 'to', 'fromtype', 'totype', 'edgeorder'))
  expect_s3_class(edges, 'tbl_df')
})
