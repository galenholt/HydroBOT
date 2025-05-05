test_that("funs", {
  remean <- function(x) {
    mean(x)
  }

  y <- remean(c(1,2,3))


  fun2 <- function(x, f) {
    a <- get(f)
    a(x)
  }

  z <- fun2(c(1,2,3), 'mean')
  b <- fun2(c(1,2,3), 'remean')



  expect_equal(y, 2)
  expect_equal(z, 2)
  expect_equal(b, 2)
})
