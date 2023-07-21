test_that("werp theme works", {
  expect_equal(class(theme_werp_toolkit()), c('theme', 'gg'))
  # with some args
  expect_equal(class(theme_werp_toolkit(base_size = 15, legend.position = 'none')), c('theme', 'gg'))
})
