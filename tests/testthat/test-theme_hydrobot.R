test_that("werp theme works", {
  expect_equal(class(theme_hydrobot()), c('theme', 'gg'))
  # with some args
  expect_equal(class(theme_hydrobot(base_size = 15, legend.position = 'none')), c('theme', 'gg'))
})
