test_that("polyID", {
  sdlid <- add_polyID(sdl_units)
  expect_snapshot(sdlid$polyID)
})
