test_that("polyID", {
  sdlid <- add_polyID(sdl_units)
  expect_snapshot(sdlid$polyID)
})

test_that("polyID with empty geoms", {
  sdle <- sdl_units
  sdle$geometry[2] <- NULL
  expect_error(sdlid <- add_polyID(sdle))
  expect_warning(sdlid <- add_polyID(sdle, failduplicate = FALSE))
  expect_snapshot(sdlid$polyID)
})
