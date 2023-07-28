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

test_that("list of ~ functions works", {
  aggchar1 <- general_aggregate(mtcars, groupers = cyl, aggCols = disp,
                                funlist = list(mean = ~mean(., na.rm = T)))
  expect_equal(names(aggchar1), c('cyl', 'agg_mean_disp'))
  expect_equal(aggchar1$cyl, c(4,6,8))
  expect_equal(round(aggchar1$agg_mean_disp, 4), c(105.1364, 183.3143, 353.1000))

  aggchar2 <- general_aggregate(mtcars, groupers = c(cyl, carb), aggCols = c(disp, wt),
                                funlist = list(mean = ~mean(., na.rm = T), min = ~min(., na.rm = T)))
  # stringr::str_flatten(names(aggchar2), "', '")
  namestring <- c('cyl', 'carb', 'agg_mean_disp', 'agg_min_disp', 'agg_mean_wt', 'agg_min_wt')
  expect_equal(names(aggchar2), namestring)
  expect_equal(aggchar2$cyl, c(4,4, 6,6, 6, 8, 8, 8, 8))
  expect_equal(aggchar2$carb, c(1,2,1,4,6,2,3,4,8))

  # lists specified externally
  anlist <- list(mean = ~mean(., na.rm = T), min = ~min(., na.rm = T))
  aggchar2 <- general_aggregate(mtcars, groupers = c(cyl, carb), aggCols = c(disp, wt),
                                funlist = anlist)
  # stringr::str_flatten(names(aggchar2), "', '")
  namestring <- c('cyl', 'carb', 'agg_mean_disp', 'agg_min_disp', 'agg_mean_wt', 'agg_min_wt')
  expect_equal(names(aggchar2), namestring)
  expect_equal(aggchar2$cyl, c(4,4, 6,6, 6, 8, 8, 8, 8))
  expect_equal(aggchar2$carb, c(1,2,1,4,6,2,3,4,8))
  # too much effort to get the others. If this becomes an issue, use something that checks an output.
})

test_that("list of \\(x) functions works", {
  aggchar1 <- general_aggregate(mtcars, groupers = cyl, aggCols = disp,
                                funlist = list(mean = \(x) mean(x, na.rm = T)))
  expect_equal(names(aggchar1), c('cyl', 'agg_mean_disp'))
  expect_equal(aggchar1$cyl, c(4,6,8))
  expect_equal(round(aggchar1$agg_mean_disp, 4), c(105.1364, 183.3143, 353.1000))

  aggchar2 <- general_aggregate(mtcars, groupers = c(cyl, carb), aggCols = c(disp, wt),
                                funlist = list(mean = \(x) mean(x, na.rm = T), min = \(x) min(x, na.rm = T)))
  # stringr::str_flatten(names(aggchar2), "', '")
  namestring <- c('cyl', 'carb', 'agg_mean_disp', 'agg_min_disp', 'agg_mean_wt', 'agg_min_wt')
  expect_equal(names(aggchar2), namestring)
  expect_equal(aggchar2$cyl, c(4,4, 6,6, 6, 8, 8, 8, 8))
  expect_equal(aggchar2$carb, c(1,2,1,4,6,2,3,4,8))

  # lists specified externally
  anlist <- list(mean = \(x) mean(x, na.rm = T), min = \(x) min(x, na.rm = T))
  aggchar2 <- general_aggregate(mtcars, groupers = c(cyl, carb), aggCols = c(disp, wt),
                                funlist = anlist)
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
  # expect warning as of dplyr 1.1- they're deprecating the use of dots- and yet
  # sometimes it *doesn't* throw a warning- having a hard time getting this test
  # to consistently pass because the warning is intermittent- I think it
  # typically does not warn if we test_file, but does when we test() the
  # package.
  expect_warning(aggchar1 <- general_aggregate(mtna, groupers = cyl, aggCols = disp,
                                funlist = mean, na.rm = TRUE))
  expect_equal(names(aggchar1), c('cyl', 'agg_mean_disp'))
  expect_equal(aggchar1$cyl, c(4,6,8))
  expect_equal(round(aggchar1$agg_mean_disp, 4), c(104.4444, 187.2000, 352.5692))

})

test_that("handling data-variables as arguments works", {

  # \(x) form of anonymous
  aggchar1 <- general_aggregate(mtcars, groupers = cyl, aggCols = disp,
                                funlist = rlang::quo(list(mean = \(x) mean(x, na.rm = TRUE),
                                               wm = \(x) weighted.mean(x, gear, na.rm = TRUE))))

  expect_equal(names(aggchar1), c('cyl', 'agg_mean_disp', 'agg_wm_disp'))
  expect_equal(aggchar1$cyl, c(4,6,8))
  expect_equal(round(aggchar1$agg_mean_disp, 4), c(105.1364, 183.3143, 353.1000))

  # same thing, but specified externally
  anlist <- rlang::quo(list(mean = \(x) mean(x, na.rm = TRUE),
                           wm = \(x) weighted.mean(x, gear, na.rm = TRUE)))

  aggchare <- general_aggregate(mtcars, groupers = cyl, aggCols = disp,
                                funlist = anlist)
  expect_equal(names(aggchare), c('cyl', 'agg_mean_disp', 'agg_wm_disp'))
  expect_equal(aggchare$cyl, c(4,6,8))
  expect_equal(round(aggchare$agg_mean_disp, 4), c(105.1364, 183.3143, 353.1000))

  # tilde form of anonymous
  aggchart <- general_aggregate(mtcars, groupers = cyl, aggCols = disp,
                                funlist = rlang::quo(list(mean = ~mean(., na.rm = TRUE),
                                                          wm = ~weighted.mean(., gear, na.rm = TRUE))))

  expect_equal(names(aggchart), c('cyl', 'agg_mean_disp', 'agg_wm_disp'))
  expect_equal(aggchart$cyl, c(4,6,8))
  expect_equal(round(aggchart$agg_mean_disp, 4), c(105.1364, 183.3143, 353.1000))

  # same thing, but specified externally
  anlistt <- rlang::quo(list(mean = ~mean(., na.rm = TRUE),
                            wm = ~weighted.mean(., gear, na.rm = TRUE)))

  aggcharte <- general_aggregate(mtcars, groupers = cyl, aggCols = disp,
                                funlist = anlistt)
  expect_equal(names(aggcharte), c('cyl', 'agg_mean_disp', 'agg_wm_disp'))
  expect_equal(aggcharte$cyl, c(4,6,8))
  expect_equal(round(aggcharte$agg_mean_disp, 4), c(105.1364, 183.3143, 353.1000))

  # mixed with bare names
  anlistb <- rlang::quo(list(mean = mean,
                            wm = \(x) weighted.mean(x, gear, na.rm = TRUE)))

  aggchareb <- general_aggregate(mtcars, groupers = cyl, aggCols = disp,
                                funlist = anlistb)
  expect_equal(names(aggchareb), c('cyl', 'agg_mean_disp', 'agg_wm_disp'))
  expect_equal(aggchareb$cyl, c(4,6,8))
  expect_equal(round(aggchareb$agg_mean_disp, 4), c(105.1364, 183.3143, 353.1000))

  # mixed with character names- this doesn't work, the quosure prevents the
  # function extraction from quoted names
  anlistc <- rlang::quo(list(mean = 'mean',
                             wm = \(x) weighted.mean(x, gear, na.rm = TRUE)))

  expect_error(aggcharec <- general_aggregate(mtcars, groupers = cyl, aggCols = disp,
                                 funlist = anlistc))

  # using named, not anonymous functions with data variables
  # This fails with the second arg buried in the function
  wem <- function(x) {weighted.mean(x, gear, na.rm = TRUE)}

  expect_error(aggcharwe <- general_aggregate(mtcars, groupers = cyl, aggCols = disp,
                                 funlist = rlang::quo(list(mean = mean, wem = wem))))


  # it works here because we quo and expose the gear argument
  wem2 <- function(x, gear) {weighted.mean(x, gear, na.rm = TRUE)}

  l2 <- rlang::quo(list(mean = mean, wm = wem2))

  aggcharwe2 <- general_aggregate(mtcars, groupers = cyl, aggCols = disp,
                                 funlist = l2)
  expect_equal(names(aggcharwe2), c('cyl', 'agg_mean_disp', 'agg_wm_disp'))
  expect_equal(aggcharwe2$cyl, c(4,6,8))
  expect_equal(round(aggcharwe2$agg_mean_disp, 4), c(105.1364, 183.3143, 353.1000))
})
