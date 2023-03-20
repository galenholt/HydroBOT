test_that("long data referencing works", {
  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs',
                                                package = 'werptoolkitr'))
  baselong <- baseline_compare(hydlong, compare_col = 'scenario', comp_fun = difference,
                               base_lev = 'base', values_col = 'flow')
  # names are right
  expect_equal(names(baselong), c('scenario', 'Date', 'gauge', 'flow', 'ref_flow', 'difference_flow'))
  # all the bases are 0
  expect_equal(sum(baselong$difference_flow[which(baselong$scenario == 'base')]), 0)
})

test_that("multiple values columns works", {
  hydwide <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs',
                                                package = 'werptoolkitr'),
                        long = FALSE)
  expect_warning(basewide <- baseline_compare(hydwide, compare_col = 'scenario', comp_fun = difference,
                               base_lev = 'base', values_col = tidyselect::starts_with('4'),
                               names_to = 'gauge', values_to = 'flow'))
  # names are right
  expect_equal(names(basewide), c('scenario', 'Date', 'gauge', 'flow', 'ref_flow', 'difference_flow'))
  # all the bases are 0
  expect_equal(sum(basewide$difference_flow[which(basewide$scenario == 'base')]), 0)
})

test_that("scalar refs works", {
  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs',
                                                package = 'werptoolkitr'))
  hydwide <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs',
                                                package = 'werptoolkitr'),
                        long = FALSE)
  basescalar <- baseline_compare(hydlong, compare_col = 'scenario', comp_fun = difference,
                               base_lev = 0, values_col = 'flow')

  expect_warning(basescalarVals <- baseline_compare(hydwide, compare_col = 'scenario',comp_fun = difference,
                                 base_lev = c(0,1,2,3,4,5), values_col = tidyselect::starts_with('4'),
                                 names_to = 'gauge', values_to = 'flow'))
  # names are right
  expect_equal(names(basescalar), c('scenario', 'Date', 'gauge', 'flow', 'ref_flow', 'difference_flow'))
  # all the refs are 0, so there shouldn't be a difference between the orig and the diff.
  expect_equal(basescalar$flow - basescalar$difference_flow, basescalar$ref_flow)
  # names are right
  expect_equal(names(basescalarVals), c('scenario', 'Date', 'gauge', 'flow', 'ref_flow', 'difference_flow'))
  # all the refs are 0, so there shouldn't be a difference between the orig and the diff.
  expect_equal(basescalarVals$flow - basescalarVals$difference_flow, basescalarVals$ref_flow)
})

test_that("relative comparison works", {

  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs',
                                                package = 'werptoolkitr'))
  baselong <- baseline_compare(hydlong, compare_col = 'scenario', comp_fun = relative,
                               base_lev = 'base', values_col = 'flow')
  # names are right
  expect_equal(names(baselong), c('scenario', 'Date', 'gauge', 'flow', 'ref_flow', 'relative_flow'))
  # all the bases should be 1 or div/0
  expect_true(all(baselong$relative_flow[which(baselong$scenario == 'base' & baselong$flow != 0)] == 1))
})

test_that("relative comparison works with add_eps", {
  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs',
                                                package = 'werptoolkitr'))
  baselong <- baseline_compare(hydlong, compare_col = 'scenario', comp_fun = relative, add_eps = 1,
                               base_lev = 'base', values_col = 'flow')
  # names are right
  expect_equal(names(baselong), c('scenario', 'Date', 'gauge', 'flow', 'ref_flow', 'relative_flow'))
  # all the bases should be 1 or div/0
  expect_equal(sum(is.nan(baselong$relative_flow)), 0)
})

test_that("character functions work", {

  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs',
                                                package = 'werptoolkitr'))
  baselong <- baseline_compare(hydlong, compare_col = 'scenario', comp_fun = 'difference',
                               base_lev = 'base', values_col = 'flow')
  # names are right
  expect_equal(names(baselong), c('scenario', 'Date', 'gauge', 'flow', 'ref_flow', 'difference_flow'))
  # all the bases are 0
  expect_equal(sum(baselong$difference_flow[which(baselong$scenario == 'base')]), 0)
})

test_that("list functions work", {

  hydlong <- read_hydro(hydropath = system.file('extdata/testsmall/hydrographs',
                                                package = 'werptoolkitr'))
  baselong <- baseline_compare(hydlong, compare_col = 'scenario', comp_fun = list(difference = ~difference(., y = ref_flow)),
                               base_lev = 'base', values_col = 'flow')
  # names are right
  expect_equal(names(baselong), c('scenario', 'Date', 'gauge', 'flow', 'ref_flow', 'difference_flow'))
  # all the bases are 0
  expect_equal(sum(baselong$difference_flow[which(baselong$scenario == 'base')]), 0)
})
