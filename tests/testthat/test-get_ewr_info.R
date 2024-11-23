test_that("Causal returns", {
  causals <- get_causal_ewr()
  expect_equal(names(causals), c('ewr2obj', 'obj2target', 'obj2yrtarget'))
  expect_s3_class(causals[[1]], 'tbl_df')
})
