# dummy functions to use to deal with nesting

dummyfun <- function(groupers, aggers, fa = T) {
  g2 <- selectcreator(rlang::enquo(groupers), mtcars, failmissing = fa)
  a2 <- selectcreator(rlang::enquo(aggers), mtcars, failmissing = fa)

  dataagg <- mtcars %>%
    dplyr::group_by(dplyr::across({{g2}})) %>%
    dplyr::summarise(dplyr::across({{a2}}, mean)) %>%
    dplyr::ungroup()
}


dummyouter <- function(groups, ags) {
  test <- dummyfun(groupers = groups, aggers = ags)
}


rlang::local_options(lifecycle_verbosity = "error")

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

# the dummyfun is actually what often happens
test_that("passtidy", {

  mta <- dummyfun(groupers = tidyselect::starts_with('ge'),
                  aggers = tidyselect::ends_with('pg'))
  expect_equal(names(mta), c('gear', 'mpg'))

})

test_that("passbare", {

  mta <- dummyfun(groupers = gear,
                  aggers = mpg)
  expect_equal(names(mta), c('gear', 'mpg'))

})

test_that("passchar", {

  mta <- dummyfun(groupers = 'gear',
                  aggers = 'mpg')
  expect_equal(names(mta), c('gear', 'mpg'))

})

# with another layer
test_that("passtidy", {

  mta <- dummyouter(groups = tidyselect::starts_with('ge'),
                    ags = tidyselect::ends_with('pg'))
  expect_equal(names(mta), c('gear', 'mpg'))

})

test_that("bare doesn't work higher in the stack", {

  expect_error(mta <- dummyouter(groups = gear,
                                 ags = mpg))

})

test_that("passchar", {

  mta <- dummyouter(groups = 'gear',
                    ags = 'mpg')
  expect_equal(names(mta), c('gear', 'mpg'))

})
