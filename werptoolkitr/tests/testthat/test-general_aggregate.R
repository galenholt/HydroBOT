test_that("characters for everything works", {
  aggchar1 <- general_aggregate(mtcars, groupers = 'cyl', aggCols = 'disp', funlist = 'mean')
  expect_equal(names(aggchar1), c('cyl', 'agg_mean_disp'))
  expect_equal(aggchar1$cyl, c(4,6,8))
  expect_equal(round(aggchar1$agg_mean_disp, 4), c(105.1364, 183.3143, 353.1000))

  aggchar2 <- general_aggregate(mtcars, groupers = c('cyl', 'carb'), aggCols = c('disp', 'wt'), funlist = c('mean', 'min'))
  # stringr::str_flatten(names(aggchar2), "', '")
  namestring <- c('cyl', 'carb', 'agg_mean_disp', 'agg_min_disp', 'agg_mean_wt', 'agg_min_wt')
  expect_equal(names(aggchar2), namestring)
  expect_equal(aggchar2$cyl, c(4,4, 6,6, 6, 8, 8, 8, 8))
  expect_equal(aggchar2$carb, c(1,2,1,4,6,2,3,4,8))
  # too much effort to get the others. If this becomes an issue, use something that checks an output.
})

test_that("bare names for everything works", {
  aggchar1 <- general_aggregate(mtcars, groupers = cyl, aggCols = disp, funlist = mean)
  expect_equal(names(aggchar1), c('cyl', 'agg_mean_disp'))
  expect_equal(aggchar1$cyl, c(4,6,8))
  expect_equal(round(aggchar1$agg_mean_disp, 4), c(105.1364, 183.3143, 353.1000))

  aggchar2 <- general_aggregate(mtcars, groupers = c(cyl, carb), aggCols = c(disp, wt), funlist = c(mean, min))
  # stringr::str_flatten(names(aggchar2), "', '")
  namestring <- c('cyl', 'carb', 'agg_mean_disp', 'agg_min_disp', 'agg_mean_wt', 'agg_min_wt')
  expect_equal(names(aggchar2), namestring)
  expect_equal(aggchar2$cyl, c(4,4, 6,6, 6, 8, 8, 8, 8))
  expect_equal(aggchar2$carb, c(1,2,1,4,6,2,3,4,8))
  # too much effort to get the others. If this becomes an issue, use something that checks an output.
})

test_that("tidyselect for groupers and aggCols works", {
  # This is more like the aggchar2s above- two groups, two variables
  aggchar <- general_aggregate(mtcars, groupers = tidyselect::starts_with('c'),
                                aggCols = tidyselect::ends_with('t'),
                                funlist = mean)
  expect_equal(names(aggchar), c('cyl', 'carb', 'agg_mean_drat', 'agg_mean_wt'))
  expect_equal(aggchar$cyl, c(4,4, 6,6, 6, 8, 8, 8, 8))
  expect_equal(aggchar$carb, c(1,2,1,4,6,2,3,4,8))

  # Any reason to think it'll fail with two functions?
  aggchar2 <- general_aggregate(mtcars, groupers = tidyselect::starts_with('c'),
                                aggCols = tidyselect::ends_with('t'),
                                funlist = c('mean', 'sd'))
  # stringr::str_flatten(names(aggchar2), "', '")
  namestring <- c('cyl', 'carb', 'agg_mean_drat', 'agg_sd_drat', 'agg_mean_wt', 'agg_sd_wt')
  expect_equal(names(aggchar2), namestring)
  expect_equal(aggchar2$cyl, c(4,4, 6,6, 6, 8, 8, 8, 8))
  expect_equal(aggchar2$carb, c(1,2,1,4,6,2,3,4,8))
  # too much effort to get the others. If this becomes an issue, use something that checks an output.
})

test_that("failmissing works", {
  aggchar1 <- general_aggregate(mtcars, groupers = c('cyl', 'not_a_col'),
                                aggCols = c('disp', 'also_not'),
                                funlist = 'mean', failmissing = FALSE)

  expect_equal(names(aggchar1), c('cyl', 'agg_mean_disp'))
  expect_equal(aggchar1$cyl, c(4,6,8))
  expect_equal(round(aggchar1$agg_mean_disp, 4), c(105.1364, 183.3143, 353.1000))

  # explicit TRUE
  expect_error(aggchar1 <- general_aggregate(mtcars, groupers = c('cyl', 'not_a_col'),
                                             aggCols = c('disp', 'also_not'),
                                             funlist = 'mean', failmissing = TRUE))

  # Default TRUE
  expect_error(aggchar1 <- general_aggregate(mtcars, groupers = c('cyl', 'not_a_col'),
                                             aggCols = c('disp', 'also_not'),
                                             funlist = 'mean'))
})

test_that("dots pass", {
  mtna <- mtcars
  mtna[c(1, 5, 9, 19), 'disp'] <- NA
  aggchar1 <- general_aggregate(mtna, groupers = cyl, aggCols = disp,
                                funlist = mean, na.rm = TRUE)
  expect_equal(names(aggchar1), c('cyl', 'agg_mean_disp'))
  expect_equal(aggchar1$cyl, c(4,6,8))
  expect_equal(round(aggchar1$agg_mean_disp, 4), c(104.4444, 187.2000, 352.5692))

})
