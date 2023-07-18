
test_that("character works", {
  mc <- selectcreator(c('gear', 'wt'), mtcars)
  expect_equal(mc, c('gear', 'wt'))
})

# Need to use the `rlang::expr` here to write these tests. typically it would have been done already.
test_that("tidyselect works", {

  selector <- rlang::expr(tidyselect::starts_with('w'))

  mt <- selectcreator(selector, mtcars)
  expect_equal(mt, 'wt')
})

test_that("tidyselect with any_of/all_of works", {
  selector <- rlang::expr(tidyselect::any_of(c('wt', 'gear', 'drat')))
  mta <- selectcreator(selector, mtcars)
  expect_equal(mta, c('wt', 'gear', 'drat'))
})

# This is how it's often called
test_that("tidyselect quoed character", {
  selcols <- c('wt', 'gear', 'drat')

  mta <- selectcreator(rlang::enquo(selcols), mtcars)
  expect_equal(mta, c('wt', 'gear', 'drat'))

})
