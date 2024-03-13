test_that("makes a named vector of colors of class 'colors'", {
  levnames <- c('one', 'two', 'three')
  simplepal <- make_pal(levnames, palette = 'calecopal::superbloom3')
  expect_equal(names(simplepal), levnames)
  expect_equal(class(simplepal), 'colors')
  expect_equal(simplepal, as_colors(stats::setNames(c("#E69512FF", "#D3105CFF", "#3B4F8EFF"), levnames)))
})


test_that("reference works", {
  levnames <- c('one', 'two', 'three')
  refpal <- make_pal(levnames, palette = 'calecopal::superbloom3', refvals = 'two', refcols = 'black')
  expect_equal(sort(refpal), sort(as_colors(stats::setNames(c("#E69512FF", "black", "#D3105CFF"), levnames))))
  expect_equal(class(refpal), 'colors')
})

test_that("all as reference works and out of order", {
  levnames <- c('one', 'two', 'three')
  refpal <- make_pal(levnames, palette = 'calecopal::superbloom3', refvals = c('two', 'three', 'one'),
                     refcols = c('black', 'forestgreen', 'purple4'))
  expect_equal(sort(refpal), sort(as_colors(stats::setNames(c('purple4', 'black', 'forestgreen'), levnames))))
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

test_that("changing direction works", {
  levnames <- c('one', 'two', 'three')
  simplepal <- make_pal(levnames, palette = 'calecopal::superbloom3', direction = -1)
  expect_equal(names(simplepal), levnames)
  expect_equal(class(simplepal), 'colors')
  expect_equal(simplepal, as_colors(stats::setNames(c("#3B4F8EFF", "#D3105CFF", "#E69512FF"), levnames)))

  # with a continuous
  simplepal_c <- make_pal(levnames, palette = 'scico::oslo', direction = 1)
  expect_equal(simplepal_c, as_colors(stats::setNames(c("#000000FF", "#4F7ABBFF", "#FFFFFFFF"), levnames)))

  simplepal_r <- make_pal(levnames, palette = 'scico::oslo', direction = -1)
  expect_equal(simplepal_r, rev(simplepal_c) |> setNames(names(simplepal_c)))

})


test_that("informative errors with unknown palettes", {

  # try just a simple case issue
  levnames <- c('one', 'two', 'three')
  err_case <- expect_error(make_pal(levnames, palette = 'ggsci::NRC_NPG'))
  expect_equal(err_case$message, "Requested palette not present, likely because wrong case or missing letters.
Try `ggsci::nrc_npg`")

  err_missing <- expect_error(make_pal(levnames, palette = 'ggsci::nrc'))
  expect_equal(err_missing$message, "Requested palette not present, likely because wrong case or missing letters.
Try `ggsci::nrc_npg`")

  err_pkg <- expect_error(make_pal(levnames, palette = 'nopkg::notapal'))
  expect_equal(err_pkg$message, "Requested palette not available in paletteer")

  # being informative about this would involve fuzzy matching, and that would
  # need to import a whole new package, so not going to bother
  # simplepal <- make_pal(levnames, palette = 'ggsci::npg')
})
