test_that("Causal returns", {
  causals <- get_causal_ewr()
  expect_type(causals, 'list')
  expect_s3_class(causals[[1]], 'tbl_df')
})

test_that("ewr table works", {
  ewrtab <- get_ewr_table()
  expect_s3_class(ewrtab, 'tbl_df')
})


test_that("version works", {
  ewrver <- get_ewr_version()

  expect_type(ewrver, 'character')
})
