test_that("makes a named vector of colors of class 'colors'", {
  levnames <- c('one', 'two', 'three')
  simplepal <- make_pal(levnames, palette = 'calecopal::superbloom3')
  expect_equal(names(simplepal), levnames)
  expect_equal(class(simplepal), 'colors')
  expect_equal(simplepal, as_colors(setNames(c("#E69512FF", "#D3105CFF", "#3B4F8EFF"), levnames)))
})


test_that("reference works", {
  levnames <- c('one', 'two', 'three')
  refpal <- make_pal(levnames, palette = 'calecopal::superbloom3', refvals = 'two', refcols = 'black')
  expect_equal(sort(refpal), sort(as_colors(setNames(c("#E69512FF", "black", "#D3105CFF"), levnames))))
  expect_equal(class(refpal), 'colors')
})

test_that("all as reference works and out of order", {
  levnames <- c('one', 'two', 'three')
  refpal <- make_pal(levnames, palette = 'calecopal::superbloom3', refvals = c('two', 'three', 'one'),
                     refcols = c('black', 'forestgreen', 'purple4'))
  expect_equal(sort(refpal), sort(as_colors(setNames(c('purple4', 'black', 'forestgreen'), levnames))))
  expect_equal(class(refpal), 'colors')
})

test_that("includeref keeps the right vals", {
  levnames <- c('one', 'two', 'three')
  refpal <- make_pal(levnames, palette = 'calecopal::superbloom3', refvals = 'two', refcols = 'black',
                     includeRef = TRUE)
  simplepal <- make_pal(levnames, palette = 'calecopal::superbloom3')

  expect_equal(refpal[c('one', 'three')], simplepal[c('one', 'three')])
  expect_equal(class(refpal), 'colors')
})

test_that("returnunref works", {
  levnames <- c('one', 'two', 'three')
  refpal <- make_pal(levnames, palette = 'calecopal::superbloom3', refvals = 'two', refcols = 'black',
                     includeRef = TRUE, returnUnref = TRUE)
  simplepal <- make_pal(levnames, palette = 'calecopal::superbloom3')
  expect_equal(refpal$unrefcols, simplepal)
  expect_equal(refpal$refcols[c('one', 'three')], simplepal[c('one', 'three')])
  expect_equal(class(refpal$refcols), 'colors')
  expect_equal(class(refpal$unrefcols), 'colors')
})
