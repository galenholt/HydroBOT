test_that("long hydrograph csv works", {
  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs', package = 'werptoolkitr'))
  expect_equal(names(hydlong), c('scenario', 'Date', 'gauge', 'flow'))
  expect_equal(class(hydlong), c('tbl_df', 'tbl', 'data.frame'))
  expect_equal(ncol(hydlong), 4)
  expect_equal(nrow(hydlong), 32850)

  hydlong_b <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs', package = 'werptoolkitr'),
                        scenariofilter = 'base_base')
  expect_equal(ncol(hydlong_b), 4)
  expect_equal(nrow(hydlong_b), 10950)

})

test_that("wide hydrograph csv works", {
  hydwide <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs', package = 'werptoolkitr'), long = FALSE)
  expect_equal(names(hydwide)[1:2], c('scenario', 'Date'))
  expect_equal(class(hydwide), c('tbl_df', 'tbl', 'data.frame'))
  expect_equal(ncol(hydwide), 8)
  expect_equal(nrow(hydwide), 5475)

})


test_that("long hydrograph netcdf works", {
  hydlong <- read_hydro(hydropath = system.file("extdata/ncdfexample/nchydros", package = 'werptoolkitr'), format = 'nc', gaugemap = 'iqqm')
  expect_equal(names(hydlong), c('scenario', 'Date', 'gauge', 'flow', 'node'))
  expect_equal(class(hydlong), c('tbl_df', 'tbl', 'data.frame'))
  expect_equal(nrow(hydlong), 24000)

  # read everything in
  hydlong <- read_hydro(hydropath = system.file("extdata/ncdfexample/nchydros", package = 'werptoolkitr'), format = 'nc', gaugemap = 'none')
  expect_equal(names(hydlong), c('scenario', 'Date', 'flow', 'node'))
  expect_equal(class(hydlong), c('tbl_df', 'tbl', 'data.frame'))
  expect_equal(nrow(hydlong), 33000)


})
