test_that("colors works", {
  expect_equal(class(as_colors(c('black', 'red'))), 'colors')
})
