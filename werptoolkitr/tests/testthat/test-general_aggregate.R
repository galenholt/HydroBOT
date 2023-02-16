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
