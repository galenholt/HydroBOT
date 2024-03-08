test_that("character works", {
  # Simple
  a <- functionlister("mean")
  expect_type(a, "list")
  expect_equal(names(a), "mean")
  expect_equal(length(a), 1)

  # multiple functions
  a <- functionlister(c("mean", "sd"))
  expect_type(a, "list")
  expect_equal(names(a), c("mean", "sd"))
  expect_equal(length(a), 2)
})

test_that("list works", {
  # simple
  a <- functionlister(list(m = ~ mean(.)))
  expect_type(a, "list")
  expect_equal(names(a), "m")
  expect_equal(length(a), 1)
  # with arguments
  a <- functionlister(list(mna = ~ mean(., na.rm = TRUE)))
  expect_type(a, "list")
  expect_equal(names(a), "mna")
  expect_equal(length(a), 1)
  # multiple functions
  a <- functionlister(list(
    mna2 = ~ mean(., na.rm = TRUE),
    sd2 = ~ sd(., na.rm = FALSE)
  ))
  expect_type(a, "list")
  expect_equal(names(a), c("mna2", "sd2"))
  expect_equal(length(a), 2)
})

test_that("bare names work", {
  # Simple
  a <- functionlister(mean)
  expect_type(a, "list")
  expect_equal(names(a), "mean")
  expect_equal(length(a), 1)

  # multiple functions
  a <- functionlister(c(mean, sd))
  expect_type(a, "list")
  expect_equal(names(a), c("mean", "sd"))
  expect_equal(length(a), 2)
})
