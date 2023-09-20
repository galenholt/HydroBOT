test_that("making edges works", {
  aggseq <- list(c('ewr_code_timing', 'ewr_code'),
                 c('ewr_code', "env_obj"),
                 c('env_obj', "Specific_goal"),
                 c('Specific_goal', 'Objective'),
                 c('Objective', 'target_5_year_2024'))

  edges <- make_edges(causal_ewr, aggseq)
})
